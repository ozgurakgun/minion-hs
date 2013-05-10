{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Minion.Builder where

import Control.Monad.Identity
import Control.Monad.State
import Data.Default
import Data.List

import Language.Minion.Definition
import Language.Minion.Print
import Language.Minion.Run


runMinionBuilder :: Monad m => MinionBuilder m () -> m Model
runMinionBuilder (MinionBuilder s) = mModel `liftM` execStateT s (MinionBuilderState 1 def)

printMinionBuilder :: MinionBuilder Identity () -> IO ()
printMinionBuilder builder = do
    let model = runIdentity $ runMinionBuilder builder
    print $ printModel model

solve :: MinionBuilder Identity () -> IO ()
solve builder = do
    let model = runIdentity $ runMinionBuilder builder
    print $ printModel model
    solution <- runMinion model
    print solution


newtype MinionBuilder m a = MinionBuilder (StateT MinionBuilderState m a)
    deriving ( Functor, Monad, MonadState MinionBuilderState )

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
        cons <- ifThenElse (Cwliteral  x            0 ) (Cwliteral out (-1)) =<<
                ifThenElse (Cwatchless x  (constant 0)) (Cwliteral out  0  ) =<<
                ifThen     (Csumgeq   [x] (constant 0)) (Cwliteral out  1  )
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


domainBool :: Monad m => MinionBuilder m DecVarDomain
domainBool = return Bool

domainBound :: Monad m => Int -> Int -> MinionBuilder m DecVarDomain
domainBound lower upper = return (Bound lower upper)

domainDiscrete :: Monad m => Int -> Int -> MinionBuilder m DecVarDomain
domainDiscrete lower upper = return (Discrete lower upper)

domainSparseBound :: Monad m => [Int] -> MinionBuilder m DecVarDomain
domainSparseBound = return . SparseBound


constant :: Int -> Flat
constant = ConstantI

mkVarHelper
    :: Monad m
    => MinionBuilder m String
    -> MinionBuilder m DecVarDomain
    -> MinionBuilder m Flat
mkVarHelper mname mdomain = do
    name   <- mname
    domain <- mdomain
    let var = (name, domain)
    model <- gets mModel
    modify $ \ st -> st { mModel = model { mVars = var : mVars model } }
    return $ DecVarRef name

varBool :: Monad m => MinionBuilder m Flat
varBool = mkVarHelper nextName domainBool

varBound :: (Functor m, Monad m) => Int -> Int -> MinionBuilder m Flat
varBound lower upper = mkVarHelper nextName $ domainBound lower upper

varDiscrete :: (Functor m, Monad m) => Int -> Int -> MinionBuilder m Flat
varDiscrete lower upper = mkVarHelper nextName $ domainDiscrete lower upper

varSparseBound :: (Functor m, Monad m) => [Int] -> MinionBuilder m Flat
varSparseBound values = mkVarHelper nextName $ domainSparseBound values


postConstraint :: Monad m => Constraint -> MinionBuilder m ()
postConstraint c = do
    model <- gets mModel
    modify $ \ st -> st { mModel = model { mCons = c : mCons model } }

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


model1 :: (Functor m, Monad m) => MinionBuilder m ()
model1 = do
    x <- varBound 1 9
    postConstraint (Cwliteral x 3)
    output x

model2 :: (Functor m, Monad m) => MinionBuilder m ()
model2 = do
    b <- varBool
    x <- varDiscrete 1 9
    y <- varDiscrete 1 9
    z <- varDiscrete 1 9
    let c1 = Calldiff [x,y]
    let c2 = Cmodulo x y z
    postConstraint c1
    postConstraint c2
    outputs [b,x,y,z]

