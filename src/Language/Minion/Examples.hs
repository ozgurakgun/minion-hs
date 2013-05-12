module Language.Minion.Examples where

import Language.Minion.Definition
import Language.Minion.Builder


model1 :: (Functor m, Monad m) => MinionBuilder m ()
model1 = do
    x <- varBound 1 9
    postConstraint (Cwliteral x 3)
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

model3 :: (Functor m, Monad m) => MinionBuilder m ()
model3 = do
    b <- varBool
    x <- varDiscrete 1 9
    y <- varDiscrete 1 9
    z <- varDiscrete 1 9
    postConstraint $ Calldiff [x,y]
    postConstraint $ Cmodulo x y z
    postConstraint $ Cwliteral z 42
    outputs [b,x,y,z]

