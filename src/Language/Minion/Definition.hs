module Language.Minion.Definition where


import Data.Default

data Model = Model
    { mVars  :: [DecVar]
    , mCons  :: [Constraint]
    , mPrint :: [String]
    }
    deriving (Eq, Show)

instance Default Model where
    def = Model [] [] []

data DecVarDomain
    = Bool
    | Bound Int Int
    | Discrete Int Int
    | SparseBound [Int]
    deriving (Eq, Show)

getDomBounds :: DecVarDomain -> (Int, Int)
getDomBounds Bool = (0, 1)
getDomBounds (Bound x y) = (x, y)
getDomBounds (Discrete x y) = (x, y)
getDomBounds (SparseBound xs) = (head xs, last xs)


type DecVar = (String, DecVarDomain)

data Flat
    = ConstantI Int
    | DecVarRef String
    deriving (Eq, Show)

data Constraint

    -- | abs(x,y) makes sure that x=|y|, i.e. x is the absolute value of y.
    = Cabs Flat Flat

    -- | ensure that each variable takes a different value
    | Calldiff [Flat]

    -- |  difference(x,y,z) ensures that z=|x-y| in any solution.
    | Cdifference Flat Flat Flat

    -- | x != y
    | Cdiseq Flat Flat

    -- | x = y
    | Ceq    Flat Flat

    -- | x <= y + k
    | Cineq  Flat Flat Int

    -- | x = -y
    | Cminuseq Flat Flat

    -- | div(x,y,z) ensures that floor(x/y)=z.
    | Cdiv Flat Flat Flat

    -- | modulo(x,y,z) ensures that x%y=z i.e. z is the remainder of
    -- dividing x by y.
    -- For negative values, we ensure that: y(x/y) + x%y = x
    | Cmodulo Flat Flat Flat

    -- | product(x,y,z) ensures that x*y=z.
    | Cproduct Flat Flat Flat

    -- | pow(x,y,z) ensures that x^y=z.
    | Cpow Flat Flat Flat

    -- | element(vec, i, e) specifies that, in any solution, vec[i] = e
    --   and i is in the range [0 .. |vec|-1].
    | Celement [Flat] Flat Flat

    -- | lexleq(vec0, vec1)
    -- takes two vectors vec0 and vec1 of the same length and ensures that
    -- vec0 is lexicographically less than or equal to vec1 in any
    -- solution.
    | Clexleq  [(Flat, Flat)]

    -- | lexless(vec0, vec1)
    -- takes two vectors vec0 and vec1 of the same length and ensures that
    -- vec0 is lexicographically less than vec1 in any solution.
    | Clexless [(Flat, Flat)]

    -- | max(vec, x)
    -- ensures that x is equal to the maximum value of any variable in vec.
    | Cmax [Flat] Flat

    -- | min(vec, x)
    -- ensures that x is equal to the minimum value of any variable in vec.
    | Cmin [Flat] Flat

    -- | occurrence(vec, elem, count)
    -- ensures that there are count occurrences of the value elem
    -- in the vector vec.
    | Coccurrence [Flat] Int Flat

    -- | occurrenceleq(vec, elem, count)
    -- ensures that there are AT MOST count occurrences of the value elem
    -- in the vector vec.
    | Coccurrenceleq [Flat] Int Flat

    -- | occurrencegeq(vec, elem, count)
    -- ensures that there are AT LEAST count occurrences of the value elem
    -- in the vector vec.
    | Coccurrencegeq [Flat] Int Flat

    -- | sumgeq(vec, c) ensures that sum(vec) >= c.
    | Csumgeq [Flat] Flat

    -- | sumleq(vec, c) ensures that sum(vec) >= c.
    | Csumleq [Flat] Flat

    -- | reify(constraint, r) where r is a 0/1 var
    -- ensures that r is set to 1 if and only if constraint is satisfied.
    | Creify Constraint Flat

    -- | watched-and({C1,...,Cn})
    -- ensures that the constraints C1,...,Cn are all true.
    | Cwatchedand [Constraint]

    -- | watched-or({C1,...,Cn})
    -- ensures that at least one of the constraints C1,...,Cn is true.
    | Cwatchedor  [Constraint]

    -- | watchelement(vec, i, e)
    -- specifies that, in any solution, vec[i] = e and i is in the
    -- range [0 .. |vec|-1].
    | Cwatchelement [Flat] Flat Flat

    -- | watchless(x,y) ensures that x is less than y.
    | Cwatchless Flat Flat

    -- | watchsumgeq(vec, c) ensures that sum(vec) >= c.
    | Cwatchsumgeq [Flat] Flat

    -- | watchsumleq(vec, c) ensures that sum(vec) >= c.
    | Cwatchsumleq [Flat] Flat

    -- | weightedsumgeq(constantVec, varVec, total)
    -- ensures that constantVec.varVec >= total, where constantVec.varVec
    -- is the scalar dot product of constantVec and varVec.
    | Cweightedsumgeq [(Int, Flat)] Flat

    -- | weightedsumleq(constantVec, varVec, total)
    -- ensures that constantVec.varVec >= total, where constantVec.varVec
    -- is the scalar dot product of constantVec and varVec.
    | Cweightedsumleq [(Int, Flat)] Flat

    -- | w-inrange(x, [a,b]) ensures that a <= x <= b.
    | Cwinrange Flat (Int,Int)

    -- | w-notinrange(x, [a,b]) ensures that x < a or b < x.
    | Cwnotinrange Flat (Int,Int)

    -- | w-inset(x, [a1,...,an]) ensures that x belongs to the set {a1,..,an}.
    | Cwinset Flat [Int]

    -- | w-notinset(x, [a1,...,an]) ensures that x does not belong to the set {a1,..,an}.
    | Cwnotinset Flat [Int]

    -- | w-literal(x, a) ensures that x=a.
    | Cwliteral Flat Int

    -- | w-notliteral(x, a) ensures that x =/= a.
    | Cwnotliteral Flat Int

    deriving (Eq, Show)


