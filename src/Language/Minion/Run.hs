{-# LANGUAGE OverloadedStrings #-}

module Language.Minion.Run where

import Shelly
import qualified Data.Text.Lazy as T


import Language.Minion.Definition
import Language.Minion.Print


data MinionOpt = FindAllSols
    deriving (Eq, Show)

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
    let opts =  [ "-findallsols" | FindAllSols `elem` useropts ]
             ++ [ "-printsolsonly", "--" ]
    stdout <- run "minion" opts
    -- liftIO $ putStrLn $ T.unpack stdout
    return
        [ read s
        | l <- T.lines stdout
        , let s = T.unpack l
        , not $ "Solution found with Value" `T.isPrefixOf` l
        ]

