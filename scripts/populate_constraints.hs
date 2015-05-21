
import Data.Char ( isAlphaNum )
import Data.List ( stripPrefix )
import Data.Either ( rights )
import Data.Maybe ( catMaybes )
import System.Process ( readProcess )


main :: IO ()
main = do
    constraints0 <- fmap lines $ readFile "populate_constraints.constraints"
    args0        <- fmap lines $ readFile "populate_constraints.args"
    let
        args = map prepArg args0
        constraints = rights
            [ case lookup c args of
                Nothing -> Left $ "Parameters unknown for constraint: " ++ c
                Just as -> Right (c,as)
            | c <- constraints0
            , all (\ ch -> isAlphaNum ch || ch `elem` "-_") c
            ]

    putStrLn "module Language.Minion.Definition.Constraint where"
    putStrLn ""
    putStrLn "import Language.Minion.Definition.Prim"
    putStrLn ""
    putStrLn "data Constraint"
    putStrLn ""
    outs <- sequence $ zipWith toHaskell ("=" : repeat "|") constraints
    mapM_ putStrLn (catMaybes outs)
    putStrLn ""
    putStrLn "    deriving (Eq, Show)"
    putStrLn ""


prepArg :: String -> (String, [String])
prepArg line =
    case stripPrefix "set(NAME_READ_" line of
        Nothing -> error $ "Unexpected line format: " ++ line
        Just rest ->
            case words (init rest) of                               -- drop the last ")"
                (cons:args) -> (cons, map read args)
                _ -> error $ "Unexpected line format: " ++ line


toHaskell :: String -> (String, [String]) -> IO (Maybe String)
toHaskell seperator (cons, args) =
    case mapM onArg args of
        Nothing    -> return Nothing
        Just args' -> do
            help <- readProcess "minion" ["help", "constraints", cons] ""
            return $ Just $ unlines $
                ("    -- | " ++ cons)
                :  map ("    -- " ++) (drop 3 $ lines help)
                ++ [unwords $ "    " : seperator : cons' : args']

    where
        cons' = "C" ++ map (\ c -> if c == '-' then '_' else c ) cons

        onArg "read_var" = Just "Flat"
        onArg "read_2_vars" = Just "Flat Flat"
        onArg "read_list" = Just "[Flat]"
        onArg "read_constant" = Just "Int"
        onArg "read_constant_list" = Just "[Int]"
        onArg "read_constraint" = Just "Constraint"
        onArg "read_constraint_list" = Just "[Constraint]"
        onArg "read_tuples" = Just "[[Int]]"
        onArg "read_short_tuples" = Nothing
        onArg s = error $ "Unknown argument: " ++ s

