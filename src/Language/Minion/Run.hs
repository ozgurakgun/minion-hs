{-|
Utility functions for running a Minion model.
-}

{-# LANGUAGE OverloadedStrings #-}

module Language.Minion.Run ( runMinion, MinionOpt(..) ) where

import Language.Minion.Definition
import Language.Minion.Print

import Shelly
import qualified Data.Text as T


-- | Run a Minion model, potentially returning multiple solutions.
--   Each solution is a list of assignments to variables.
--   The variable names will be auto-generated, they are here mostly for debugging.
--   However, the order of them will match the order provided in 'Language.Minion.mPrint'.
runMinion :: [MinionOpt] -> Model -> IO [[(String, Int)]]
runMinion opts model@(Model _ _ _ outs _) = do
    let len = length outs
    let
        chunk _ [] = []
        chunk i xs = take i xs : chunk i (drop i xs)
    results <- runMinionPrim opts (show $ printModel model)
    return $ chunk len (zip (cycle $ reverse outs) results)

runMinionPrim :: [MinionOpt] -> String -> IO [Int]
runMinionPrim useropts model = shelly $ silently $ print_stdout False $ do
    -- liftIO $ putStrLn model
    setStdin $ T.pack model
    let opts =  concatMap prepOption useropts
             ++ [ "-printsolsonly", "--" ]
    stdout <- run "minion" opts
    -- liftIO $ putStrLn $ T.unpack stdout
    return
        [ read s
        | l <- T.lines stdout
        , let s = T.unpack l
        , not $ "Solution found with Value" `T.isPrefixOf` l
        ]


-- | Minion options.
data MinionOpt
    = FindAllSols               -- ^ Find all solutions. This option is ignored if the
                                -- problem contains any minimising or maximising objective.
    | TimeLimit Int             -- ^ To stop search after N seconds (real time)
    | CpuLimit Int              -- ^ To stop search after N seconds (CPU time)
    | NodeLimit Int             -- ^ To stop search after N nodes
    | SolLimit Int              -- ^ To stop search after N solutions have been found
    deriving (Eq, Show)

prepOption :: MinionOpt -> [T.Text]
prepOption FindAllSols   = [ "-findallsols" ]
prepOption (TimeLimit n) = [ "-timelimit" , T.pack $ show n ]
prepOption (CpuLimit  n) = [ "-cpulimit"  , T.pack $ show n ]
prepOption (NodeLimit n) = [ "-nodelimit" , T.pack $ show n ]
prepOption (SolLimit  n) = [ "-sollimit"  , T.pack $ show n ]
