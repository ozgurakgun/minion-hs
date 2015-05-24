{-|
This module defines the representations of domains, decision variables and constants in Minion.
-}

module Language.Minion.Definition.Prim
    ( DecVarDomain(..)
    , getDomBounds
    , DecVar
    , Flat(..)
    ) where


-- | The domain of a decision variable in Minion.
data DecVarDomain
    = Bool                      -- ^ Boolean domains. Decision variables with boolean domanins are used very
                                --   commonly for logical expressions, and for encoding the characteristic
                                --   functions of sets and relations.
    | Bound Int Int             -- ^ Bounds variables, where only the upper and lower bounds of the domain
                                --   are maintained. These domains must be continuous ranges of integers
                                --   i.e. holes cannot be put in the domains of the variables.
    | Discrete Int Int          -- ^ In discrete variables, the domain ranges between the specified lower and upper
                                --   bounds, but during search any domain value may be pruned, i.e., propagation and
                                --   search may punch arbitrary holes in the domain.
    | SparseBound [Int]         -- ^ In sparse bounds variables the domain is composed of discrete values
                                --   (e.g. {1, 5, 36, 92}), but only the upper and lower bounds of the
                                --   domain may be updated during search. Although the domain of these
                                --   variables is not a continuous range, any holes in the domains must be
                                --   there at time of specification, as they can not be added during the
                                --   solving process.
    deriving (Eq, Show)

-- | A utility function to get the defined lower and upper bounds of a domain.
getDomBounds :: DecVarDomain -> (Int, Int)
getDomBounds Bool = (0, 1)
getDomBounds (Bound x y) = (x, y)
getDomBounds (Discrete x y) = (x, y)
getDomBounds (SparseBound xs) = (head xs, last xs)

-- | A decision variable is represented as a pair of a name and a domain.
type DecVar = (String, DecVarDomain)

-- | A flat expression represents either an integer constant or a reference to a decision variable.
data Flat
    = ConstantI Int             -- ^ An integer constant
    | DecVarRef String          -- ^ Reference to a decision variable
    deriving (Eq, Show)
