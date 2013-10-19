{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Minion.Builder
    ( MinionBuilder, runMinionBuilder, solve
    , sanitiseName
    , domainBool, domainBound, domainDiscrete, domainSparseBound
    , varBool, varBool'
    , varBound, varBound'
    , varSparseBound, varSparseBound'
    , varDiscrete, varDiscrete'
    , varVector, varVector'
    , minimising, maximising
    , postConstraint
    , ifThen, ifThenElse
    , output, outputs, searchOrder
    , constant, pure
    , cWeightedSumEq
    ) where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.State
import Data.Char ( isAlphaNum, isDigit )
import Data.Default
import Data.List
import qualified Data.Map as M

import Language.Minion.Definition
import Language.Minion.Print
import Language.Minion.Run


runMinionBuilder :: Monad m => MinionBuilder m () -> m Model
runMinionBuilder (MinionBuilder s) = mModel `liftM` execStateT s (MinionBuilderState 1 def)

solve :: MinionBuilder Identity () -> IO ()
solve builder = do
    let model = runIdentity $ runMinionBuilder builder
    print $ printModel model
    solution <- runMinion [] model
    putStrLn $ "Number of solutions: " ++ show (length solution)
    mapM_ print solution


newtype MinionBuilder m a = MinionBuilder (StateT MinionBuilderState m a)
    deriving ( Functor, Applicative, Monad, MonadState MinionBuilderState )

instance (Functor m, Monad m) => Num (MinionBuilder m Flat) where

    fromInteger = return . constant . fromInteger

    abs mx = do
        x <- mx
        (xMin, xMax) <- boundsOf x
        let allValues = nub $ sort
                        [ xVal
                        | xVal <- [xMin..xMax]
                        , xVal >= 0
                        ]
        out <- varDiscrete (minimum allValues) (maximum allValues)
        let cons = Cwatchedand [ Cabs x out
                               , Cwinset out allValues
                               ]
        postConstraint cons
        return out

    signum mx = do
        x <- mx
        out  <- varDiscrete (-1) 1
        cons <- ifThenElse (Cwliteral  x            0 ) (Cwliteral out   0 ) =<<
                ifThenElse (Cwatchless x  (constant 0)) (Cwliteral out (-1))
                                                        (Cwliteral out   1 )
        postConstraint cons
        return out

    mx + my = do
        x <- mx
        y <- my
        (xMin, xMax) <- boundsOf x
        (yMin, yMax) <- boundsOf x
        out <- varDiscrete (xMin + yMin) (xMax + yMax)
        let cons = Cwatchedand [ Csumleq [x,y] out
                               , Csumgeq [x,y] out
                               ]
        postConstraint cons
        return out

    mx * my = do
        x <- mx
        y <- my
        (xMin, xMax) <- boundsOf x
        (yMin, yMax) <- boundsOf x
        let allValues = nub $ sort
                        [ xVal * yVal
                        | xVal <- [xMin..xMax]
                        , yVal <- [yMin..yMax]
                        ]
        out <- varDiscrete (minimum allValues) (maximum allValues)
        let cons = Cwatchedand [ Cproduct x y out
                               , Cwinset out allValues
                               ]
        postConstraint cons
        return out


data MinionBuilderState = MinionBuilderState
    { mVarCount :: Int
    , mModel    :: Model
    }

nextName :: Monad m => MinionBuilder m String
nextName = do
    i <- gets mVarCount
    modify $ \ st -> st { mVarCount = i + 1 }
    return ("v_" ++ show i)

boundsOf :: Monad m => Flat -> MinionBuilder m (Int, Int)
boundsOf (ConstantI x) = return (x, x)
boundsOf (DecVarRef x) = do
    m <- gets mModel
    case x `lookup` mVars m of
        Nothing -> error $ "Undefined decision variable: " ++ x
        Just dom -> return $ getDomBounds dom


domainBool :: DecVarDomain
domainBool = Bool

domainBound :: Int -> Int -> DecVarDomain
domainBound = Bound

domainDiscrete :: Int -> Int -> DecVarDomain
domainDiscrete = Discrete

domainSparseBound :: [Int] -> DecVarDomain
domainSparseBound = SparseBound


constant :: Int -> Flat
constant = ConstantI

mkVarHelper
    :: Monad m
    => String
    -> DecVarDomain
    -> MinionBuilder m Flat
mkVarHelper name domain = do
    let var = (name, domain)
    model <- gets mModel
    modify $ \ st -> st { mModel = model { mVars = var : mVars model } }
    return $ DecVarRef name

varBool :: Monad m => MinionBuilder m Flat
varBool = do
    name <- nextName
    mkVarHelper name domainBool

varBool' :: Monad m => String -> MinionBuilder m Flat
varBool' (sanitiseName -> name) = do
    output (DecVarRef name)
    mkVarHelper name domainBool

varBound :: (Functor m, Monad m) => Int -> Int -> MinionBuilder m Flat
varBound lower upper = do
    name <- nextName
    mkVarHelper name $ domainBound lower upper

varBound' :: (Functor m, Monad m) => String -> Int -> Int -> MinionBuilder m Flat
varBound' (sanitiseName -> name) lower upper = do
    output (DecVarRef name)
    mkVarHelper name $ domainBound lower upper

varDiscrete :: (Functor m, Monad m) => Int -> Int -> MinionBuilder m Flat
varDiscrete lower upper = do
    name <- nextName
    mkVarHelper name $ domainDiscrete lower upper

varDiscrete' :: (Functor m, Monad m) => String -> Int -> Int -> MinionBuilder m Flat
varDiscrete' (sanitiseName -> name) lower upper = do
    output (DecVarRef name)
    mkVarHelper name $ domainDiscrete lower upper

varSparseBound :: (Functor m, Monad m) => [Int] -> MinionBuilder m Flat
varSparseBound values = do
    name <- nextName
    mkVarHelper name $ domainSparseBound values

varSparseBound' :: (Functor m, Monad m) => String -> [Int] -> MinionBuilder m Flat
varSparseBound' (sanitiseName -> name) values = do
    output (DecVarRef name)
    mkVarHelper name $ domainSparseBound values

varVector :: (Show ix, Ord ix, Monad m) => DecVarDomain -> [ix] -> MinionBuilder m (ix -> Flat, [Flat])
varVector domain indices = do
    name <- nextName
    list <- forM indices $ \ ix -> do
        let name' = sanitiseName (name ++ "_" ++ show ix)
        v <- mkVarHelper name' domain
        return (ix, v)
    let theMap = M.fromList list
    let lookupFunc i = case i `M.lookup` theMap of
            Nothing -> error $ "varVector1D, unknown index: " ++ show i
            Just x  -> x
    return (lookupFunc, map snd list)

varVector' :: (Show ix, Ord ix, Monad m) => String -> DecVarDomain -> [ix] -> MinionBuilder m (ix -> Flat, [Flat])
varVector' name domain indices = do
    list <- forM indices $ \ ix -> do
        let name' = sanitiseName (name ++ "_" ++ show ix)
        v <- mkVarHelper name' domain
        output (DecVarRef name')
        return (ix, v)
    let theMap = M.fromList list
    let lookupFunc i = case i `M.lookup` theMap of
            Nothing -> error $ "varVector1D, unknown index: " ++ show i
            Just x  -> x
    return (lookupFunc, map snd list)


setObjHelper :: Monad m => Maybe Objective -> MinionBuilder m ()
setObjHelper objective = do
    model <- gets mModel
    modify $ \ st -> st { mModel = model { mObj = objective }}

minimising :: Monad m => Flat -> MinionBuilder m ()
minimising (ConstantI _   ) = setObjHelper Nothing
minimising (DecVarRef name) = setObjHelper (Just (Minimising name))

maximising :: Monad m => Flat -> MinionBuilder m ()
maximising (ConstantI _   ) = setObjHelper Nothing
maximising (DecVarRef name) = setObjHelper (Just (Maximising name))


class PostConstraint a where
    postConstraint :: Monad m => a -> MinionBuilder m ()

instance PostConstraint Constraint where
    postConstraint c = do
        model <- gets mModel
        modify $ \ st -> st { mModel = model { mCons = c : mCons model } }

instance PostConstraint constraint => PostConstraint [constraint] where
    postConstraint cs = mapM_ postConstraint cs

reifyConstraint :: Monad m => Constraint -> MinionBuilder m Flat
reifyConstraint c = do
    var <- varBool
    postConstraint (Creify c var)
    return var

ifThenElse :: Monad m => Constraint -> Constraint -> Constraint -> MinionBuilder m Constraint
ifThenElse condition thenCase elseCase = do
    ifTrue    <- reifyConstraint condition
    ifFalse   <- reifyConstraint $ Cwliteral ifTrue 0
    thenCase' <- reifyConstraint thenCase
    elseCase' <- reifyConstraint elseCase
    return $ Cwatchedand
        [ Cineq ifTrue  thenCase' 0
        , Cineq ifFalse elseCase' 0
        ]

ifThen :: Monad m => Constraint -> Constraint -> MinionBuilder m Constraint
ifThen condition thenCase = do
    ifTrue    <- reifyConstraint condition
    thenCase' <- reifyConstraint thenCase
    return $ Cineq ifTrue thenCase' 0

output :: Monad m => Flat -> MinionBuilder m ()
output (ConstantI _) = return ()
output (DecVarRef x) = do
    model <- gets mModel
    modify $ \ st -> st { mModel = model { mPrint = x : mPrint model } }

outputs :: Monad m => [Flat] -> MinionBuilder m ()
outputs = mapM_ output


searchOrder :: Monad m => [(Flat,AscDesc)] -> MinionBuilder m ()
searchOrder so = do
    model <- gets mModel
    modify $ \ st -> st { mModel = model { mSearchOrder = [ (nm, ad) | (DecVarRef nm, ad) <- so ] } }



------------------------------------------------------------
------------------------------------------------------------

sanitiseName :: String -> String
sanitiseName = firstLetter . map toUnderscore
    where
        toUnderscore ch | isAlphaNum ch = ch
                        | otherwise     = '_'

        firstLetter xs@(x:_) | isDigit x  = '_':xs
                             | otherwise  = xs
        firstLetter [] = error "empty name"


------------------------------------------------------------
------------------------------------------------------------

cAnd :: [Constraint] -> Constraint
cAnd ls = Cwatchedand ls

cWeightedSumLeq :: [(Int, Flat)] -> Flat -> Constraint
cWeightedSumLeq ls x = Cweightedsumleq ls x

cWeightedSumGeq :: [(Int, Flat)] -> Flat -> Constraint
cWeightedSumGeq ls x = Cweightedsumgeq ls x

cWeightedSumEq :: [(Int, Flat)] -> Flat -> Constraint
cWeightedSumEq ls x = cAnd [cWeightedSumLeq ls x, cWeightedSumGeq ls x]



