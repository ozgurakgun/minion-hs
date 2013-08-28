{-# LANGUAGE OverloadedStrings #-}

module Language.Minion.Print where

import Data.List ( (\\) )
import Text.PrettyPrint

import Language.Minion.Definition


printModel :: Model -> Doc
printModel (Model vars cons obj prints) = vcat
    $  [ "MINION 3" , "**VARIABLES**" ]
    ++ map printVar  (reverse vars)
    ++ [ "**CONSTRAINTS**" ]
    ++ map printCons (reverse cons)
    ++ [ "**SEARCH**"
       , "PRINT" <+> inBrackets (commaSep $ map inBrackets userVars)
       , "VARORDER STATIC" <+> inBrackets (commaSep userVars)
       , "VARORDER AUX" <+> inBrackets (commaSep auxVars)
       ]
    ++ [ printObj o | Just o <- [obj] ]
    ++ [ "**EOF**" ]
    where
        userVars = map text $ reverse prints
        auxVars = map text $ map fst vars \\ prints
        commaSep xs = fsep $ punctuate "," xs
        inBrackets x = "[" <> x <> "]"

printObj :: Objective -> Doc
printObj (Minimising name) = "MINIMISING" <+> text name
printObj (Maximising name) = "MAXIMISING" <+> text name

printVar :: DecVar -> Doc
printVar (name, Bool)
    = "BOOL" <+> text name
printVar (name, Bound lower upper)
    = "BOUND" <+> text name <+>
      "{" <> int lower <> ".." <> int upper <> "}"
printVar (name, Discrete lower upper)
    = "DISCRETE" <+> text name <+>
      "{" <> int lower <> ".." <> int upper <> "}"
printVar (name, SparseBound values)
    = "SPARSEBOUND" <+> text name <+>
      "{" <> fsep (punctuate "," (map int values)) <> "}"

printCons :: Constraint -> Doc
printCons (Cabs a b) = printGenericCons "abs" (map printFlat [a,b])
printCons (Calldiff xs) = printGenericCons "alldiff" [printVector xs]
printCons (Cdifference a b c) = printGenericCons "difference" (map printFlat [a,b,c])
printCons (Cdiseq a b) = printGenericCons "diseq" (map printFlat [a,b])
printCons (Ceq a b) = printGenericCons "eq" (map printFlat [a,b])
printCons (Cineq a b c) = printGenericCons "ineq" [printFlat a, printFlat b, int c]
printCons (Cminuseq a b) = printGenericCons "minuseq" (map printFlat [a,b])
printCons (Cdiv a b c) = printGenericCons "div" (map printFlat [a,b,c])
printCons (Cmodulo a b c) = printGenericCons "modulo" (map printFlat [a,b,c])
printCons (Cproduct a b c) = printGenericCons "product" (map printFlat [a,b,c])
printCons (Cpow a b c) = printGenericCons "pow" (map printFlat [a,b,c])
printCons (Celement as b c) = printGenericCons "element" [printVector as, printFlat b, printFlat c]
printCons (Clexless xs) = printGenericCons "lexless" [printVector as, printVector bs] where (as,bs) = unzip xs
printCons (Clexleq  xs) = printGenericCons "lexleq"  [printVector as, printVector bs] where (as,bs) = unzip xs
printCons (Cmax as b) = printGenericCons "max" [printVector as, printFlat b]
printCons (Cmin as b) = printGenericCons "min" [printVector as, printFlat b]
printCons (Coccurrence as b c) = printGenericCons "occurrence" [printVector as, int b, printFlat c]
printCons (Coccurrenceleq as b c) = printGenericCons "occurrenceleq" [printVector as, int b, printFlat c]
printCons (Coccurrencegeq as b c) = printGenericCons "occurrencegeq" [printVector as, int b, printFlat c]
printCons (Csumleq as b) = printGenericCons "sumleq" [printVector as, printFlat b]
printCons (Csumgeq as b) = printGenericCons "sumgeq" [printVector as, printFlat b]
printCons (Creify a b) = printGenericCons "reify" [printCons a, printFlat b]
printCons (Cwatchedand as) = "watched-and({" <> fsep (punctuate "," (map printCons as)) <> "})"
printCons (Cwatchedor  as) = "watched-or({"  <> fsep (punctuate "," (map printCons as)) <> "})"
printCons (Cwatchelement as b c) = printGenericCons "watchelement" [printVector as, printFlat b, printFlat c]
printCons (Cwatchless a b) = printGenericCons "watchless" (map printFlat [a,b])
printCons (Cwatchsumleq as b) = printGenericCons "watchsumleq" [printVector as, printFlat b]
printCons (Cwatchsumgeq as b) = printGenericCons "watchsumgeq" [printVector as, printFlat b]
printCons (Cweightedsumleq xs c) = printGenericCons "weightedsumleq" [printVector (map ConstantI as), printVector bs, printFlat c] where (as,bs) = unzip xs
printCons (Cweightedsumgeq xs c) = printGenericCons "weightedsumgeq" [printVector (map ConstantI as), printVector bs, printFlat c] where (as,bs) = unzip xs
printCons (Cwinrange a (l,u)) = printGenericCons "w-inrange" [printFlat a, printVector [ConstantI l, ConstantI u]]
printCons (Cwnotinrange a (l,u)) = printGenericCons "w-notinrange" [printFlat a, printVector [ConstantI l, ConstantI u]]
printCons (Cwinset a bs) = printGenericCons "w-inset" [printFlat a, printVector (map ConstantI bs)]
printCons (Cwnotinset a bs) = printGenericCons "w-notinset" [printFlat a, printVector (map ConstantI bs)]
printCons (Cwliteral a b) = printGenericCons "w-literal" [printFlat a, int b]
printCons (Cwnotliteral a b) = printGenericCons "w-notliteral" [printFlat a, int b]
-- printCons x = text $ show x

printFlat :: Flat -> Doc
printFlat (ConstantI x) = int x
printFlat (DecVarRef x) = text x

printGenericCons :: String -> [Doc] -> Doc
printGenericCons name args = text name <> "(" <> fsep (punctuate "," args) <> ")"

printVector :: [Flat] -> Doc
printVector xs = "[" <> fsep (punctuate "," (map printFlat xs)) <> "]"


