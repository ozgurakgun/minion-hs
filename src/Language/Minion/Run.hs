{-# LANGUAGE OverloadedStrings #-}

module Language.Minion.Run where

import Shelly
import qualified Data.Text.Lazy as T

import Language.Minion.Definition
import Language.Minion.Print


runMinion :: Model -> IO [(String, Int)]
runMinion model@(Model _ _ outs) = do
    results <- runMinionPrim (show $ printModel model)
    return (zip (reverse outs) results)

runMinionPrim :: String -> IO [Int]
runMinionPrim model = shelly $ silently $ print_stdout False $ do
    setStdin $ T.pack model
    stdout <- run "minion" ["-printsolsonly", "--"]
    return $ map (read . T.unpack) $ T.lines stdout

