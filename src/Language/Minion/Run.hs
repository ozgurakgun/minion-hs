{-# LANGUAGE OverloadedStrings #-}

module Language.Minion.Run where

import Shelly
import qualified Data.Text.Lazy as T

import Language.Minion.Definition
import Language.Minion.Print


runMinion :: Model -> IO [[(String, Int)]]
runMinion model@(Model _ _ outs) = do
    let len = length outs
    let
        chunk _ [] = []
        chunk i xs = take i xs : chunk i (drop i xs)
    results <- runMinionPrim (show $ printModel model)
    return $ chunk len (zip (cycle $ reverse outs) results)

runMinionPrim :: String -> IO [Int]
runMinionPrim model = shelly $ silently $ print_stdout False $ do
    setStdin $ T.pack model
    stdout <- run "minion" ["-findallsols", "-printsolsonly", "--"]
    return $ map (read . T.unpack) $ T.lines stdout

