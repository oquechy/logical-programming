import SAT
import Unification
import qualified MiniKanren as MK
import Lists

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

t1 = Func "f" [Func "g" [Var "a"           , Func "f" [Var "c"]], Var "b"]
t2 = Func "f" [Func "g" [Func "f" [Var "d"], Var "a"           ], Var "a"]

unit_can_unify :: Assertion
unit_can_unify = unify [] t1 t2 @?= Just [("b",Func "f" [Var "d"]),("c",Var "d"),("a",Func "f" [Var "d"])]

t3 = Func "f" [Func "g" [Func "g" [Var "d"], Var "a"           ], Var "a"]

unit_cant_unify :: Assertion
unit_cant_unify = unify [] t1 t3 @?= Nothing

t4 = Func "f" []
t5 = Var "a"

unit_unify_var_func :: Assertion
unit_unify_var_func = unify [] t4 t5 @?= Just [("a", Func "f" [])]

unit_kanren_appendo_nil :: Assertion
unit_kanren_appendo_nil = MK.solve (appendo (Var "a") (Var "b") nil) 
                            @?= [[("b",Func "nil" []),("a",Func "nil" [])]]

unit_kanren_appendo_none :: Assertion
unit_kanren_appendo_none = MK.solve (appendo (cons (Var "a") (Var "b")) (Var "c") nil) 
                            @?= []

unit_kanren_appendo_inf :: Assertion
unit_kanren_appendo_inf = let sol = MK.solve (appendo (Var "x") (cons (Var "a") (Var "b")) (Var "c")) 
                          in (length (take 100 sol) @?= 100) 
                            >> (take 1 sol @?= [[("c",Func "cons" [Var "a",Var "b"]),("x",Func "nil" [])]])

main :: IO Counts
main = runTestTT $ TestList $ map TestCase [ unit_tseytin
                                           , unit_dpll_sat
                                           , unit_dpll_unsat
                                           , unit_solve
                                           , unit_can_unify
                                           , unit_cant_unify
                                           , unit_unify_var_func 
                                           , unit_kanren_appendo_nil
                                           , unit_kanren_appendo_none 
                                           , unit_kanren_appendo_inf 
                                           ]


