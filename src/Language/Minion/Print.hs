{-|
Utility functions for printing a Minion model.
-}

{-# LANGUAGE OverloadedStrings #-}

module Language.Minion.Print ( printModel ) where

import Language.Minion.Definition

import Data.List ( (\\) )
import Text.PrettyPrint


-- | Function to print a complete Minion model.
printModel :: Model -> Doc
printModel (Model vars cons obj prints searchOrder') = vcat
    $  [ "MINION 3" , "**VARIABLES**" ]
    ++ map printVar  (reverse vars)
    ++ [ "**CONSTRAINTS**" ]
    ++ map printCons (reverse cons)
    ++ [ "**SEARCH**"
       , "PRINT" <+> inBrackets (commaSep $ map (inBrackets . text) (reverse prints))
       , "VARORDER STATIC" <+> inBrackets (commaSep $ map (text . fst) searchOrder)
       , "VALORDER" <+> inBrackets (commaSep $ map (printAscDesc . snd) searchOrder)
       , "VARORDER AUX" <+> inBrackets (commaSep auxVars)
       ]
    ++ [ printObj o | Just o <- [obj] ]
    ++ [ "**EOF**" ]
    where
        searchOrder =
            if null searchOrder'
                then zip (reverse prints) (repeat Asc) -- using the print list
                else searchOrder'
        auxVars = map text $ map fst vars \\ map fst searchOrder
        commaSep xs = fsep $ punctuate "," xs
        inBrackets x = "[" <> x <> "]"

printAscDesc :: AscDesc -> Doc
printAscDesc Asc = "a"
printAscDesc Desc = "d"

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
printCons (Calldiffmatrix xs a) = printGenericCons "alldiffmatrix" [printVector xs, int a]
printCons (Cdifference a b c) = printGenericCons "difference" (map printFlat [a,b,c])
printCons (Cdiseq a b) = printGenericCons "diseq" (map printFlat [a,b])
printCons (Cdiv a b c) = printGenericCons "div" (map printFlat [a,b,c])
printCons (Celement as b c) = printGenericCons "element" [printVector as, printFlat b, printFlat c]
printCons (Celement_one as b c) = printGenericCons "element_one" [printVector as, printFlat b, printFlat c]
printCons (Ceq a b) = printGenericCons "eq" (map printFlat [a,b])
printCons (Cgacalldiff xs) = printGenericCons "gacalldiff" [printVector xs]
printCons (Cgacschema vec vals) = printGenericCons "gacschema" [printVector vec, printSet vals]
printCons (Cgcc as bs cs) = printGenericCons "gcc" [printVector as, printVectorI bs, printVector cs]
printCons (Cgccweak as bs cs) = printGenericCons "gccweak" [printVector as, printVectorI bs, printVector cs]
printCons (Chamming as bs c) = printGenericCons "hamming" [printVector as, printVector bs, int c]
printCons (Cineq a b c) = printGenericCons "ineq" [printFlat a, printFlat b, int c]
printCons (Clexleq  as bs) = printGenericCons "lexleq"  [printVector as, printVector bs]
printCons (Clexless as bs) = printGenericCons "lexless" [printVector as, printVector bs]
printCons (Clighttable vec vals) = printGenericCons "lighttable" [printVector vec, printSet vals]
printCons (Clitsumgeq as bs c) = printGenericCons "litsumgeq" [printVector as, printVectorI bs, int c]
printCons (Cmax as b) = printGenericCons "max" [printVector as, printFlat b]
printCons (Cmddc vec vals) = printGenericCons "mddc" [printVector vec, printSet vals]
printCons (Cmin as b) = printGenericCons "min" [printVector as, printFlat b]
printCons (Cminuseq a b) = printGenericCons "minuseq" (map printFlat [a,b])
printCons (Cmodulo a b c) = printGenericCons "modulo" (map printFlat [a,b,c])
printCons (Cnegativemddc vec vals) = printGenericCons "negativemddc" [printVector vec, printSet vals]
printCons (Cnegativetable vec vals) = printGenericCons "negativetable" [printVector vec, printSet vals]
printCons (Coccurrence as b c) = printGenericCons "occurrence" [printVector as, int b, printFlat c]
printCons (Coccurrencegeq as b c) = printGenericCons "occurrencegeq" [printVector as, int b, int c]
printCons (Coccurrenceleq as b c) = printGenericCons "occurrenceleq" [printVector as, int b, int c]
printCons (Cpow a b c) = printGenericCons "pow" (map printFlat [a,b,c])
printCons (Cproduct a b c) = printGenericCons "product" (map printFlat [a,b,c])
printCons (Creify a b) = printGenericCons "reify" [printCons a, printFlat b]
printCons (Creifyimply a b) = printGenericCons "reifyimply" [printCons a, printFlat b]
printCons (Cstr2plus vec vals) = printGenericCons "str2plus" [printVector vec, printSet vals]
printCons (Csumgeq as b) = printGenericCons "sumgeq" [printVector as, printFlat b]
printCons (Csumleq as b) = printGenericCons "sumleq" [printVector as, printFlat b]
printCons (Ctable vec vals) = printGenericCons "table" [printVector vec, printSet vals]
printCons (Cwatched_and as) = "watched-and({" <> fsep (punctuate "," (map printCons as)) <> "})"
printCons (Cwatched_or  as) = "watched-or({"  <> fsep (punctuate "," (map printCons as)) <> "})"
printCons (Cwatchelement as b c) = printGenericCons "watchelement" [printVector as, printFlat b, printFlat c]
printCons (Cwatchelement_one as b c) = printGenericCons "watchelement_one" [printVector as, printFlat b, printFlat c]
printCons (Cwatchless a b) = printGenericCons "watchless" (map printFlat [a,b])
printCons (Cwatchsumgeq as b) = printGenericCons "watchsumgeq" [printVector as, int b]
printCons (Cwatchsumleq as b) = printGenericCons "watchsumleq" [printVector as, int b]
printCons (Cwatchvecneq as bs) = printGenericCons "watchvecneq" [printVector as, printVector bs]
printCons (Cweightedsumgeq as bs c) = printGenericCons "weightedsumgeq" [printVector (map ConstantI as), printVector bs, printFlat c]
printCons (Cweightedsumleq as bs c) = printGenericCons "weightedsumleq" [printVector (map ConstantI as), printVector bs, printFlat c]
printCons (Cw_inintervalset a bs) = printGenericCons "w_inintervalset" [printFlat a, printVectorI bs]
printCons (Cw_inrange a [l,u]) = printGenericCons "w-inrange" [printFlat a, printVector [ConstantI l, ConstantI u]]
printCons (Cw_inset a bs) = printGenericCons "w-inset" [printFlat a, printVector (map ConstantI bs)]
printCons (Cw_literal a b) = printGenericCons "w-literal" [printFlat a, int b]
printCons (Cw_notinrange a [l,u]) = printGenericCons "w-notinrange" [printFlat a, printVector [ConstantI l, ConstantI u]]
printCons (Cw_notinset a bs) = printGenericCons "w-notinset" [printFlat a, printVector (map ConstantI bs)]
printCons (Cw_notliteral a b) = printGenericCons "w-notliteral" [printFlat a, int b]
printCons Cw_inrange{} = error "Cw_inrange constraint must contain 2 values!"
printCons Cw_notinrange{} = error "Cw_notinrange constraint must contain 2 values!"

printFlat :: Flat -> Doc
printFlat (ConstantI x) = int x
printFlat (DecVarRef x) = text x

printGenericCons :: String -> [Doc] -> Doc
printGenericCons name args = text name <> "(" <> fsep (punctuate "," args) <> ")"

printVectorI :: [Int] -> Doc
printVectorI xs = "[" <> fsep (punctuate "," (map int xs)) <> "]"

printVector :: [Flat] -> Doc
printVector xs = "[" <> fsep (punctuate "," (map printFlat xs)) <> "]"

printSet :: [[Int]] -> Doc
printSet xs = "{" <> fsep (punctuate "," (map printVectorIs xs)) <> "}"
    where
        printVectorIs ys = "<" <> fsep (punctuate "," (map int ys)) <> ">"

