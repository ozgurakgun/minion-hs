module Language.Minion.Definition.Model where

import Data.Default
import Language.Minion.Definition.Prim
import Language.Minion.Definition.Constraint


data Model = Model
    { mVars  :: [DecVar]
    , mCons  :: [Constraint]
    , mObj   :: Maybe Objective
    , mPrint :: [String]
    , mSearchOrder :: [(String,AscDesc)]
    }
    deriving (Eq, Show)

instance Default Model where
    def = Model [] [] Nothing [] []

data AscDesc = Asc | Desc
    deriving (Eq, Show)

data Objective = Minimising String | Maximising String
    deriving (Eq, Show)
