{-|
This module reexports everything in the package.
-}

module Language.Minion
    (
    -- * Data structures for representing a Minion models
      Model(..)
    , DecVarDomain(..)
    , getDomBounds
    , DecVar
    , AscDesc(..)
    , Flat(..)
    , Objective(..)
    , Constraint(..)

    -- * Helpers for building a Minion model
    , module Language.Minion.Builder

    -- * Running a Minion model
    , Language.Minion.Builder.solve
    , runMinion, runMinion_, MinionOpt(..)

    -- * Printing a Minion model
    , printModel

    ) where

import Language.Minion.Definition
import Language.Minion.Builder hiding ( solve )
import qualified Language.Minion.Builder
import Language.Minion.Print
import Language.Minion.Run
