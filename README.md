# Logical Programming

## HW 1: SAT-Solver
```bash
$ stack run sat # run SAT-solver app
$ stack test    # tests
```

## HW 2: Unification
```bash
$ stack run unify # run unification app
$ stack test      # tests
```

## HW 4,5: MiniKanren with Disunificaton
```bash
$ stack ghci                                             # run interactively
Specify main module to use (press enter to load none): 1 # load module kanren
```
```haskell 
λ> MK.solve $ (Var "a") === (Var "b") &&& (Var "a") =/= (Var "c") -- solve A == B & A /= C
[([("a",Var "b")],[("b",Var "c")])]                               -- single solution:
                                                                  -- substite A -> B where B /= C 
-- X ++ cons A B == C
λ> sols = MK.solve (appendo (Var "x") (cons (Var "a") (Var "b")) (Var "c")) 
λ> take 3 $ materialize sols (Var "c")                            -- materialize C:
[(Func "cons" [Var "a",Var "b"],[])                               -- all lists ending on cons A B
,(Func "cons" [Var "v0",Func "cons" [Var "a",Var "b"]],[])
,(Func "cons" [Var "v0",Func "cons" [Var "v3",Func "cons" [Var "a",Var "b"]]],[])]

-- cons H nil ++ T != cons H T' -> cons V2 nil ++ T != cons V2 V3 where T /= V3
λ> sols = MK.solve (notAppendo (cons (Var "h") nil) (Var "t") (cons (Var "h") (Var "t'")))
λ> materialize sols (cons (Var "h") nil)                          -- cons H nil -> cons V2 nil
[(Func "cons" [Var "v2",Func "nil" []],[("t",Var "v3")])]         
λ> materialize sols (Var "t")                                     -- T          -> T
[(Var "t",[("t",Var "v3")])]
λ> materialize sols (cons (Var "h") (Var "t'"))                   -- cons H T'  -> cons V2 V3
[(Func "cons" [Var "v2",Var "v3"],[("t",Var "v3")])]

```

    


 
