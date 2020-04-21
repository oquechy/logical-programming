module Lists 
    ( cons
    , nil
    , appendo) where

import MiniKanren

nil :: Term
nil = Func "nil" []

cons :: Term -> Term -> Term
cons x xs = Func "cons" [x, xs]

appendo :: Term -> Term -> Term -> Goal
appendo a b c =
   (a === nil &&& b === c) |||
   (fresh $ \h -> fresh $ \t -> fresh $ \tb ->
    a === cons h t &&& c === cons h tb &&& appendo t b tb)
