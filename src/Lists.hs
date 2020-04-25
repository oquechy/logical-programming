module Lists 
    ( cons
    , nil
    , appendo
    , notAppendo
    ) where

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

notAppendo :: Term -> Term -> Term -> Goal
notAppendo a b c =
    (a === nil &&& b =/= c) |||
    (fresh $ \x -> fresh $ \xs -> a === cons x xs &&& c === nil) |||
    (fresh $ \x -> fresh $ \xs -> fresh $ \y -> fresh $ \ys ->
     a === cons x xs &&& c === cons y ys &&&
        (x =/= y ||| notAppendo xs b ys))
