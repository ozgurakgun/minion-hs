{-|
This module defines a monad transformer for building Minion models.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Minion.Builder
    (
    -- * A monad transformer to build Minion models
      MinionBuilder, runMinionBuilder, solve

    -- * Creating decisiong variables
    , varBool, varBound, varSparseBound, varDiscrete, varVector

    -- * Creating decisiong variables with a given name (these may be removed)
    , varBool', varBound', varSparseBound', varDiscrete', varVector'

    -- * Objective function
    , minimising, maximising

    -- * Controlling which variables to output in a solution
    , output, outputs

    -- * Controlling the search order in a model
    , searchOrder

    -- * Functions to use when referring to constants and references to decision variables
    , constant, pure

    -- * Posting constraints
    , postConstraint, postConstraints
    , reifyConstraint
    , ifThen, ifThenElse

    -- * Derived constraints
    , cWeightedSumEq, cWeightedSumLeq, cWeightedSumGeq

    ) where

import Language.Minion.Definition
import Language.Minion.Print
import Language.Minion.Run

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.State
import Data.Char ( isAlphaNum, isDigit )
import Data.Default
import Data.List
import qualified Data.Map as M


-- | Run the 'MinionBuilder' monad transformer and return the produced model.
--   The model can be sent to Minion using 'Language.Minion.runMinion'.
runMinionBuilder :: Monad m => MinionBuilder m () -> m Model
runMinionBuilder (MinionBuilder s) = mModel `liftM` execStateT s (MinionBuilderState 1 def)

-- | A function to directly run a 'MinionBuilder' and print the solutions to stdout.
solve :: MinionBuilder Identity () -> IO ()
solve builder = do
    let model = runIdentity $ runMinionBuilder builder
    print $ printModel model
    solution <- runMinion [] model
    putStrLn $ "Number of solutions: " ++ show (length solution)
    mapM_ print solution


-- | A monad transformer which can be used to help building a Minion model.
--
--   See 'runMinionBuilder' for a function to extract the model.
--
--   Note: 'Flat's inside a 'MinionBuilder' will have a 'Num' instance.
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
        let cons = Cwatched_and [ Cabs x out
                                , Cw_inset out allValues
                                ]
        postConstraint cons
        return out

    negate mx = do
        x <- mx
        (xMin, xMax) <- boundsOf x
        let allValues = nub $ sort
                        [ negate xVal
                        | xVal <- [xMin..xMax]
                        ]
        out <- varDiscrete (minimum allValues) (maximum allValues)
        let cons = Cwatched_and [ Cminuseq x out
                                , Cw_inset out allValues
                                ]
        postConstraint cons
        return out

    signum mx = do
        x <- mx
        out  <- varDiscrete (-1) 1
        cons <- ifThenElse (Cw_literal x            0 ) (Cw_literal out   0 ) =<<
                ifThenElse (Cwatchless x  (constant 0)) (Cw_literal out (-1))
                                                        (Cw_literal out   1 )
        postConstraint cons
        return out

    mx + my = do
        x <- mx
        y <- my
        (xMin, xMax) <- boundsOf x
        (yMin, yMax) <- boundsOf x
        out <- varDiscrete (xMin + yMin) (xMax + yMax)
        let cons = Cwatched_and [ Csumleq [x,y] out
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
        let cons = Cwatched_and [ Cproduct x y out
                                , Cw_inset out allValues
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


-- | Lift an integer constant to a 'Flat'.
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

-- | Creating a decision variable with a 'Language.Minion.Bool' domain.
varBool :: Monad m => MinionBuilder m Flat
varBool = do
    name <- nextName
    mkVarHelper name domainBool

-- | Creating a decision variable with a 'Language.Minion.Bool' domain, and with a given name.
varBool' :: Monad m => String -> MinionBuilder m Flat
varBool' (sanitiseName -> name) = do
    output (DecVarRef name)
    mkVarHelper name domainBool

-- | Creating a decision variable with a 'Language.Minion.Bound' domain.
varBound :: (Functor m, Monad m) => Int -> Int -> MinionBuilder m Flat
varBound lower upper = do
    name <- nextName
    mkVarHelper name $ domainBound lower upper

-- | Creating a decision variable with a 'Language.Minion.Bound' domain, and with a given name.
varBound' :: (Functor m, Monad m) => String -> Int -> Int -> MinionBuilder m Flat
varBound' (sanitiseName -> name) lower upper = do
    output (DecVarRef name)
    mkVarHelper name $ domainBound lower upper

-- | Creating a decision variable with a 'Language.Minion.Discrete' domain.
varDiscrete :: (Functor m, Monad m) => Int -> Int -> MinionBuilder m Flat
varDiscrete lower upper = do
    name <- nextName
    mkVarHelper name $ domainDiscrete lower upper

-- | Creating a decision variable with a 'Language.Minion.Discrete' domain, and with a given name.
varDiscrete' :: (Functor m, Monad m) => String -> Int -> Int -> MinionBuilder m Flat
varDiscrete' (sanitiseName -> name) lower upper = do
    output (DecVarRef name)
    mkVarHelper name $ domainDiscrete lower upper

-- | Creating a decision variable with a 'Language.Minion.SparseBound' domain.
varSparseBound :: (Functor m, Monad m) => [Int] -> MinionBuilder m Flat
varSparseBound values = do
    name <- nextName
    mkVarHelper name $ domainSparseBound values

-- | Creating a decision variable with a 'Language.Minion.SparseBound' domain, and with a given name.
varSparseBound' :: (Functor m, Monad m) => String -> [Int] -> MinionBuilder m Flat
varSparseBound' (sanitiseName -> name) values = do
    output (DecVarRef name)
    mkVarHelper name $ domainSparseBound values

-- | Creating a vector of decision variables.
varVector :: (Show ix, Ord ix, Monad m) => DecVarDomain -> [ix] -> MinionBuilder m (ix -> Flat, [Flat])
varVector domain indices = do
    name <- nextName
    list <- forM indices $ \ ix -> do
        let name' = sanitiseName (name ++ "_" ++ show ix)
        v <- mkVarHelper name' domain
        return (ix, v)
    let theMap = M.fromList list
    let lookupFunc i = case i `M.lookup` theMap of
            Nothing -> error $ "varVector, unknown index: " ++ show i
            Just x  -> x
    return (lookupFunc, map snd list)

-- | Creating a vector of decision variables, with a given name.
varVector' :: (Show ix, Ord ix, Monad m) => String -> DecVarDomain -> [ix] -> MinionBuilder m (ix -> Flat, [Flat])
varVector' name domain indices = do
    list <- forM indices $ \ ix -> do
        let name' = sanitiseName (name ++ "_" ++ show ix)
        v <- mkVarHelper name' domain
        output (DecVarRef name')
        return (ix, v)
    let theMap = M.fromList list
    let lookupFunc i = case i `M.lookup` theMap of
            Nothing -> error $ "varVector, unknown index: " ++ show i
            Just x  -> x
    return (lookupFunc, map snd list)


setObjHelper :: Monad m => Maybe Objective -> MinionBuilder m ()
setObjHelper objective = do
    model <- gets mModel
    modify $ \ st -> st { mModel = model { mObj = objective }}

-- | Add a 'Minimising' objective to a Minion model.
--   This will overwrite any previously added objectives.
minimising :: Monad m => Flat -> MinionBuilder m ()
minimising (ConstantI _   ) = setObjHelper Nothing
minimising (DecVarRef name) = setObjHelper (Just (Minimising name))

-- | Add a 'Maximising' objective to a Minion model.
--   This will overwrite any previously added objectives.
maximising :: Monad m => Flat -> MinionBuilder m ()
maximising (ConstantI _   ) = setObjHelper Nothing
maximising (DecVarRef name) = setObjHelper (Just (Maximising name))


class PostConstraint a where
    -- | Add a constraint to the underlying model.
    --
    --   This function works on both a single constraint and a list of constraints.
    postConstraint :: Monad m => a -> MinionBuilder m ()

instance PostConstraint Constraint where
    postConstraint c = do
        model <- gets mModel
        modify $ \ st -> st { mModel = model { mCons = c : mCons model } }

instance PostConstraint constraint => PostConstraint [constraint] where
    postConstraint cs = mapM_ postConstraint cs

-- | Given a list of values and a function to produce a constraint for each value,
--   produce and post constraints for each value.
postConstraints :: (Monad m, PostConstraint constraint) => [a] -> (a -> constraint) -> MinionBuilder m ()
postConstraints xs f = postConstraint $ map f xs

-- | Reify a constraint and return a 'Bool' representing the constraint's truth value.
reifyConstraint :: Monad m => Constraint -> MinionBuilder m Flat
reifyConstraint c = do
    var <- varBool
    postConstraint (Creify c var)
    return var

-- | If the first constraint is true, the second constraint will be posted.
--
--   If the first constraint is false, the third constraint will be posted.
ifThenElse :: Monad m => Constraint -> Constraint -> Constraint -> MinionBuilder m Constraint
ifThenElse condition thenCase elseCase = do
    ifTrue    <- reifyConstraint condition
    ifFalse   <- reifyConstraint $ Cw_literal ifTrue 0
    thenCase' <- reifyConstraint thenCase
    elseCase' <- reifyConstraint elseCase
    return $ Cwatched_and
        [ Cineq ifTrue  thenCase' 0
        , Cineq ifFalse elseCase' 0
        ]

-- | If the first constraint is true, the second will be posted too.
ifThen :: Monad m => Constraint -> Constraint -> MinionBuilder m Constraint
ifThen condition thenCase = do
    ifTrue    <- reifyConstraint condition
    thenCase' <- reifyConstraint thenCase
    return $ Cineq ifTrue thenCase' 0

-- | Include the value of the given decision variable in solutions.
--   Note that the order of the values in a solution will match the order of calls to this function.
output :: Monad m => Flat -> MinionBuilder m ()
output (ConstantI _) = return ()
output (DecVarRef x) = do
    model <- gets mModel
    modify $ \ st -> st { mModel = model { mPrint = x : mPrint model } }

-- | This function is similar to 'output', but works on a list of decision variables.
outputs :: Monad m => [Flat] -> MinionBuilder m ()
outputs = mapM_ output


-- | Specify a search order for Minion to use.
--
--   This function should only be called at most once since it overwrites any previously specified search orders.
--
--   By default all the output variables will be searched in ascending order.
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

-- | Weighted Sum Less-Than-Or-Equal
cWeightedSumLeq :: [(Int, Flat)] -> Flat -> Constraint
cWeightedSumLeq ls x = Cweightedsumleq (map fst ls) (map snd ls) x

-- | Weighted Sum Greater-Than-Or-Equal
cWeightedSumGeq :: [(Int, Flat)] -> Flat -> Constraint
cWeightedSumGeq ls x = Cweightedsumgeq (map fst ls) (map snd ls) x

-- | Weighted Sum Equal
cWeightedSumEq :: [(Int, Flat)] -> Flat -> Constraint
cWeightedSumEq ls x = Cwatched_and [cWeightedSumLeq ls x, cWeightedSumGeq ls x]



