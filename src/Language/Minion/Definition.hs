{-|
This module reexports all the definitions related to a Minion model.
-}

module Language.Minion.Definition
    ( Model(..)
    , DecVarDomain(..)
    , getDomBounds
    , DecVar
    , AscDesc(..)
    , Flat(..)
    , Objective(..)
    , Constraint(..)
    ) where

import Language.Minion.Definition.Prim
import Language.Minion.Definition.Constraint
import Language.Minion.Definition.Model
