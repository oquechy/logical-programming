module MiniKanren 
    ( Term(Func, Var)
    , Goal
    , (===)
    , (&&&)
    , (|||)
    , fresh
    , solve) where

import Data.List
import Data.Maybe
import Control.Monad

import Unification

type State = (Subst, Int)
type Goal = State -> [State] 

infix 4 ===
infix 4 =/=
infixr 3 &&&
infixr 2 |||

(===) :: Term -> Term -> Goal
(===) a b (s, v) = case unify s a b of 
                    Nothing -> []
                    Just s' -> [(s', v)]

(=/=) :: Term -> Term -> Goal
(=/=) a b (s, v) = undefined

(&&&) :: Goal -> Goal -> Goal
(&&&) a b s = a s >>= b

interleave :: [a] -> [a] -> [a]
interleave []     ys = ys
interleave (x:xs) ys = x : interleave ys xs

(|||) :: Goal -> Goal -> Goal
(|||) a b s = a s `interleave` b s

fresh :: (Term -> Goal) -> Goal
fresh f (s, v) = f (Var $ 'v':show v) (s, v + 1)

solve :: Goal -> [Subst]
solve g = map fst $ g ([], 0)
