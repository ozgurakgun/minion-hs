{-# LANGUAGE OverloadedStrings #-}

module Language.Minion.Run ( runMinion, MinionOpt(..) ) where

import Shelly
import qualified Data.Text.Lazy as T


import Language.Minion.Definition
import Language.Minion.Print


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
    setStdin $ T.pack model
    let opts =  map prepOption useropts
             ++ [ "-printsolsonly", "--" ]
    stdout <- run "minion" opts
    -- liftIO $ putStrLn $ T.unpack stdout
    return
        [ read s
        | l <- T.lines stdout
        , let s = T.unpack l
        , not $ "Solution found with Value" `T.isPrefixOf` l
        ]


data MinionOpt
    = FindAllSols
    | TimeLimit Int
    | CpuLimit Int
    | NodeLimit Int
    | SolLimit Int
    deriving (Eq, Show)

prepOption :: MinionOpt -> T.Text
prepOption FindAllSols   = "-findallsols"
prepOption (TimeLimit n) = T.pack $ "-timelimit " ++ show n
prepOption (CpuLimit  n) = T.pack $ "-cpulimit "  ++ show n
prepOption (NodeLimit n) = T.pack $ "-nodelimit " ++ show n
prepOption (SolLimit  n) = T.pack $ "-sollimit "  ++ show n
