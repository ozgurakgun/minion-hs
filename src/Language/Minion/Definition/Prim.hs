module Language.Minion.Definition.Prim where


data DecVarDomain
    = Bool
    | Bound Int Int
    | Discrete Int Int
    | SparseBound [Int]
    deriving (Eq, Show)

getDomBounds :: DecVarDomain -> (Int, Int)
getDomBounds Bool = (0, 1)
getDomBounds (Bound x y) = (x, y)
getDomBounds (Discrete x y) = (x, y)
getDomBounds (SparseBound xs) = (head xs, last xs)

type DecVar = (String, DecVarDomain)

data Flat
    = ConstantI Int
    | DecVarRef String
    deriving (Eq, Show)
