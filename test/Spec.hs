import SAT

import Data.List
import Test.HUnit
import Debug.Trace

f = ((V "p" :\/ V "q") :/\ V "r") :=> Not (V "s")

unit_tseytin :: Assertion
unit_tseytin =
  -- ((p \/ q) /\ r) -> !s
  tseytin f
    @?= [[L "A"] -- A 
        ,[L "B",L "E",N "A"],[N "B",L "A"],[N "E",L "A"] -- A := B \/ E
        ,[N "B",N "C"],[L "B",L "C"] -- B := !C
        ,[N "D",N "r",L "C"],[L "D",N "C"],[L "r",N "C"] -- C := D /\ r 
        ,[L "p",L "q",N "D"],[N "p",L "D"],[N "q",L "D"] -- D := p \/ q
        ,[N "E",N "s"],[L "E",L "s"]] -- E := !s


unit_dpll_sat :: Assertion
unit_dpll_sat =
  sort <$> dpll [[N "x", N "y"], [L "x", N "y"], [N "x", L "z"]]
    @?= sort <$> [ [("y", False), ("x", False)]
                 , [("y", False), ("x", True), ("z", True)]]

unit_dpll_unsat :: Assertion
unit_dpll_unsat =
  dpll [[N "x", N "y"], [N "x", L "y"], [L "x", N "y"], [L "y", N "z"], [L "x", L "z"]]
    @?= []

allAssignments [] = [[]] 
allAssignments (n:ns) = [(n, b):rec | b <- [True, False], rec <- allAssignments ns]

unit_solve :: Assertion
unit_solve = let a = solve f in all (eval f) a @?= True 

main :: IO Counts
main = runTestTT $ TestList $ map TestCase [ unit_tseytin
                                           , unit_dpll_sat
                                           , unit_dpll_unsat
                                           , unit_solve
                                           ]


