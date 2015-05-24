module Language.Minion.Examples where

import Language.Minion


model1 :: Monad m => MinionBuilder m ()
model1 = do
    x <- varBound 1 9
    postConstraint (Cw_literal x 3)
    output x

model2 :: (Functor m, Monad m) => MinionBuilder m ()
model2 = do
    x <- varDiscrete 1 9
    y <- varDiscrete 1 9
    z <- varDiscrete 1 9
    theSum <- pure x + pure y + pure z
    postConstraint $ Calldiff [x,y]
    postConstraint $ Cmodulo x y z
    postConstraint $ Cwatchless theSum (constant 10)
    postConstraint $ Cmodulo theSum (constant 2) (constant 0)
    outputs [x,y,z,theSum]

model3 :: Monad m => MinionBuilder m ()
model3 = do
    b <- varBool
    x <- varDiscrete 1 9
    y <- varDiscrete 1 9
    z <- varDiscrete 1 9
    postConstraint $ Calldiff [x,y]
    postConstraint $ Cmodulo x y z
    postConstraint $ Cw_literal z 42
    outputs [b,x,y,z]

model4 :: (Functor m, Monad m) => MinionBuilder m ()
model4 = do
    x <- varDiscrete (-2) 2
    postConstraint $ Cw_inset x [-2,0,2]
    y <- signum $ pure x
    outputs [x,y]

model5 :: (Functor m, Monad m) => MinionBuilder m ()
model5 = do
    x <- varDiscrete (-10) 10
    y <- varDiscrete (-10) 10
    z <- pure x * pure y
    k <- signum $ pure z
    postConstraint $ Cw_inset z [-5,5]
    outputs [x,y,k]

