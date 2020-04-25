module MiniKanren 
    ( Term(Func, Var)
    , Goal
    , (===)
    , (=/=)
    , (&&&)
    , (|||)
    , fresh
    , solve) where

import Data.Foldable 
import Data.List
import Data.Maybe
import Control.Monad

import Unification

type ConstrStore = Subst
type State = (Subst, ConstrStore, Int)
type Goal = State -> [State] 

infix 4 ===
infix 4 =/=
infixr 3 &&&
infixr 2 |||

updateConstr :: Subst -> (Term, Term) -> ConstrStore -> [ConstrStore]
updateConstr s (a, b) c = case unify [] (walk s a) (walk s b) of
                        Nothing -> [c]
                        Just s' -> map (:c) s'   

(===) :: Term -> Term -> Goal
(===) a b (s, c, v) = case unify [] (walk s a) (walk s b) of 
                        Nothing -> []
                        Just s' -> (\c -> (s' ++ s, c, v)) <$> cs s' 
    where cs s' = foldrM (updateConstr s') [] $ (\(v, t) -> (Var v, t)) <$> c    
            
(=/=) :: Term -> Term -> Goal
(=/=) a b (s, c, v) = map (\c -> (s, c, v)) (updateConstr s (a, b) c)

(&&&) :: Goal -> Goal -> Goal
(&&&) a b s = a s >>= b

interleave :: [a] -> [a] -> [a]
interleave []     ys = ys
interleave (x:xs) ys = x : interleave ys xs

(|||) :: Goal -> Goal -> Goal
(|||) a b s = a s `interleave` b s

fresh :: (Term -> Goal) -> Goal
fresh f (s, c, v) = f (Var $ 'v':show v) (s, c, v + 1)

solve :: Goal -> [(Subst, ConstrStore)]
solve g = map (\(s, c, _) -> (s, c)) $ g ([], [], 0)
