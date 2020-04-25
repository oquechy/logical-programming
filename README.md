# Logical Programming

## HW 1: SAT-Solver
    stack run sat # run SAT-solver app
    stack test    # tests

## HW 2: Unification
    stack run unify # run unification app
    stack test      # tests

## HW 4,5: MiniKanren with Disunificaton
    stack ghci # run interactively
    Specify main module to use (press enter to load none): 1 # load module kanren
    λ> MK.solve $ (Var "a") === (Var "b")
    [([("a",Var "b")],[])]
    


 
