{-|
This module defines the representation of a complete Minion model.
-}

module Language.Minion.Definition.Model
    ( Model(..)
    , AscDesc(..)
    , Objective(..)
    ) where

import Language.Minion.Definition.Prim
import Language.Minion.Definition.Constraint

import Data.Default


-- | The representation of a constraint model.
--   It can be solved using 'Language.Minion.Run.runMinion'.
data Model = Model
    { mVars  :: [DecVar]                    -- ^ Decision variables
    , mCons  :: [Constraint]                -- ^ Constraints
    , mObj   :: Maybe Objective             -- ^ Optionally an objective
    , mPrint :: [String]                    -- ^ Decision variable names to be printed in a solution
    , mSearchOrder :: [(String,AscDesc)]    -- ^ Search order, one per decision variable
    }
    deriving (Eq, Show)

instance Default Model where
    def = Model [] [] Nothing [] []

-- | Search order for a single decision variable: ascending/descending.
--   See 'Language.Minion.searchOrder'.
data AscDesc = Asc | Desc
    deriving (Eq, Show)

-- | The representation of the objective function.
--   Use 'Language.Minion.minimising' or 'Language.Minion.minimising' to construct an objective.
data Objective = Minimising String | Maximising String
    deriving (Eq, Show)
