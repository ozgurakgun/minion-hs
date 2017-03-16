{-|
Utility functions for running a Minion model.
-}

{-# LANGUAGE OverloadedStrings #-}

module Language.Minion.Run ( runMinion, runMinion_, MinionOpt(..), VarOrder(..) ) where

import Language.Minion.Definition
import Language.Minion.Print

import Control.Monad ( void )
import Data.List ( isPrefixOf )
import Data.Sequence ( (|>) )
import Data.Foldable ( toList )
import GHC.IO.Handle -- ( hIsEOF, hClose, hGetLine )
-- import System.IO.Unsafe

import Shelly
import qualified Data.Text as T


-- | Run a Minion model, potentially returning multiple solutions.
--   Each solution is a list of assignments to variables.
--   The variable names will be auto-generated, they are here mostly for debugging.
--   However, the order of them will match the order provided in 'Language.Minion.mPrint'.
runMinion :: String -> [MinionOpt] -> Model -> ([(String, Int)] -> IO a) -> IO [a]
runMinion fn useropts model@(Model _ _ _ outs' _) act = shelly $ verbosely $ print_stdout False $ do
    let outs = reverse outs'
    let outsLen = length outs
    let opts =  concatMap prepOption useropts
             -- ++ [ "-printsolsonly", "--" ]
             ++ [ "-printsolsonly", "temp" `mappend` T.pack fn ]
    let
        handl accumLen accum h | accumLen == outsLen = do
            this <- liftIO $ act $ zip outs (toList accum)
            rest <- handl 0 mempty h
            return (this : rest)
        handl accumLen accum h = do
            eof <- liftIO $ hIsEOF h
            if eof
                then return []
                else do
                    line <- liftIO $ hGetLine h
                    if "#" `isPrefixOf` line || "Solution found with Value" `isPrefixOf` line
                        then handl  accumLen     accum               h
                        else handl (accumLen+1) (accum |> read line) h
    -- setStdin $ T.pack $ show $ printModel model
    liftIO $ writeFile ("temp" ++ fn) $ show $ printModel model
    runHandle "minion" opts (handl 0 mempty)


-- | Similar to 'runMinion', but the results of the action-per-solution are not accumulated.
runMinion_ :: [MinionOpt] -> Model -> ([(String, Int)] -> IO ()) -> IO ()
runMinion_ opts model act = void $ runMinion "" opts model act


-- | Minion options.
data MinionOpt
    = FindAllSols               -- ^ Find all solutions. This option is ignored if the
                                -- problem contains any minimising or maximising objective.
    | TimeLimit Int             -- ^ To stop search after N seconds (real time)
    | CpuLimit Int              -- ^ To stop search after N seconds (CPU time)
    | NodeLimit Int             -- ^ To stop search after N nodes
    | SolLimit Int              -- ^ To stop search after N solutions have been found
    | RandomiseOrder            -- ^ Randomise both variable and value orders
    | VarOrder VarOrder         -- ^ Variable ordering heuristic to use
    deriving (Eq, Show)

data VarOrder
    = Sdf           -- ^ smallest domain first, break ties lexicographically
    | SdfRandom     -- ^ sdf, but break ties randomly
    | Srf           -- ^ smallest ratio first, chooses unassigned variable with smallest
                    --   percentage of its initial values remaining, break ties lexicographically
    | SrfRandom     -- ^ srf, but break ties randomly
    | Ldf           -- ^ largest domain first, break ties lexicographically
    | LdfRandom     -- ^ ldf, but break ties randomly
    | Random        -- ^ random variable ordering
    | Static        -- ^ lexicographical ordering
    deriving (Eq, Show)

prepOption :: MinionOpt -> [T.Text]
prepOption FindAllSols    = [ "-findallsols" ]
prepOption (TimeLimit n)  = [ "-timelimit" , T.pack $ show n ]
prepOption (CpuLimit  n)  = [ "-cpulimit"  , T.pack $ show n ]
prepOption (NodeLimit n)  = [ "-nodelimit" , T.pack $ show n ]
prepOption (SolLimit  n)  = [ "-sollimit"  , T.pack $ show n ]
prepOption RandomiseOrder = [ "-randomiseorder" ]
prepOption (VarOrder o)   = [ "-varorder"
                            , case o of
                                Sdf       -> "sdf"
                                SdfRandom -> "sdf-random"
                                Srf       -> "srf"
                                SrfRandom -> "srf-random"
                                Ldf       -> "ldf"
                                LdfRandom -> "ldf-random"
                                Random    -> "random"
                                Static    -> "static"
                            ]

