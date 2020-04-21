module Unification 
    ( Term (..)
    , Subst
    , walk
    , unify
    ) where

import Data.Functor
import Data.List
import Data.Maybe
import Control.Monad

import Debug.Trace

data Term = Var String | Func String [Term] deriving (Read, Show, Eq)
type Subst = [(String, Term)]

walk :: Subst -> Term -> Term
walk s (Var v)
    | Just t <- lookup v s = walk s t
    | otherwise            = Var v
walk s (Func f ts)         = Func f $ walk s <$> ts 

occursCheck :: String -> Term -> Bool
occursCheck v (Var u)     = v == u
occursCheck v (Func _ ts) = any (occursCheck v) ts

unify :: Subst -> Term -> Term -> Maybe Subst
unify s t1 t2 = case (walk s t1, walk s t2) of
    (Var a, Var b)                   -> Just (if a == b then s else (a, Var b):s)
    (Var a, t)                       -> guard (not $ occursCheck a t) $> (a, t):s
    (t, Var a)                       -> guard (not $ occursCheck a t) $> (a, t):s
    (Func f [], Func g [])           -> guard (f == g) $> s 
    (Func f (t:ts), Func g (t':ts')) -> guard (f == g) >> unify s t t' >>= \s' -> unify s' (Func f ts) (Func f ts')
    _                                -> Nothing
