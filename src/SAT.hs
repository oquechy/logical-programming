module SAT 
    ( Formula (..)
    , Assignment
    , Literal (..)
    , CNF
    , eval
    , dpll
    , solve
    , vars
    , tseytin
    ) where

import Data.List
import Debug.Trace 

data Formula = T
             | F 
             | V String
             | Not Formula 
             | Formula :/\ Formula
             | Formula :\/ Formula
             | Formula :=> Formula
             | Formula :<=> Formula 
             deriving (Read, Show) 

type Assignment = [(String, Bool)]

eval :: Formula -> Assignment -> Bool
eval T          _ = True
eval F          _ = False
eval (V x) a 
    | Just b <- lookup x a = b
    | otherwise            = True
eval (Not f)    a = not $ eval f a
eval (f :/\ g)  a = eval f a && eval g a
eval (f :\/ g)  a = eval f a || eval g a
eval (f :=> g)  a = eval (Not f :\/ g) a
eval (f :<=> g) a = eval ((f :=> g) :/\ (g :=> f)) a

vars :: Formula -> [String]
vars (V x)      = [x]
vars (Not f)    = vars f
vars (f :/\ g)  = vars f `union` vars g
vars (f :\/ g)  = vars f `union` vars g
vars (f :=> g)  = vars f `union` vars g
vars (f :<=> g) = vars f `union` vars g
vars _          = []

data Literal = L String | N String deriving (Show, Eq)
type CNF = [[Literal]]

eqNot :: String -> String -> CNF
eqNot n f = [[N n, N f], [L n, L f]]
eqAnd, eqOr :: String -> String -> String -> CNF
eqAnd n f g = [[N f, N g, L n], [L f, N n], [L g, N n]]
eqOr  n f g = [[L f, L g, N n], [N f, L n], [N g, L n]]

tseytin :: Formula -> CNF
tseytin f = let (cnf, subf, _) = tseytin' (names \\ vars f) f
            in [L subf] : cnf
    where alpha :: [String]
          alpha = map (: []) ['A'..'Z'] 

          names :: [String]
          names = alpha ++ [n ++ c | n <- names, c <- alpha]  

tseytin' :: [String] -> Formula -> (CNF, String, [String])
tseytin' ns  (V x)         = ([], x, ns)
tseytin' (n:ns)  T         = ([[L n]], n, ns)
tseytin' (n:m:ns)  F       = (eqNot m n ++ [[N n]], m, ns)
tseytin' (n:ns) (Not f)    = let (cnf, subf, ns') = tseytin' ns f 
                             in ((eqNot n subf) ++ cnf, n, ns')
tseytin' (n:ns) (f :/\ g)  = let (cnf, subf, ns') = tseytin' ns f in
                             let (cnf', subg, ns'') = tseytin' ns' g
                             in (eqAnd n subf subg ++ cnf ++ cnf', n, ns'')
tseytin' (n:ns) (f :\/ g)  = let (cnf, subf, ns') = tseytin' ns f in
                             let (cnf', subg, ns'') = tseytin' ns' g 
                             in (eqOr n subf subg ++ cnf ++ cnf', n, ns'')        
tseytin' ns (f :=> g)      = tseytin' ns (Not f :\/ g)
tseytin' (n:ns) (f :<=> g) = let (cnf, subfg, ns') = tseytin' ns (f :=> g) in
                             let (cnf', subgf, ns'') = tseytin' ns' (g :=> f)
                             in (eqAnd n subfg subgf ++ cnf ++ cnf', n, ns'')

dpll :: CNF -> [Assignment]
dpll = dpll' []

setTrue :: Literal -> (String, Bool)
setTrue (L n) = (n, True)
setTrue (N n) = (n, False)

setFalse :: Literal -> (String, Bool)
setFalse (L n) = (n, False)
setFalse (N n) = (n, True)

evalLit :: Literal -> Assignment -> Maybe Bool
evalLit (L n) a = lookup n a 
evalLit (N n) a = not <$> lookup n a

unitProp :: Assignment -> CNF -> Assignment
unitProp a cnf = unionBy (\a b -> fst a == fst b) a $ cnf >>= (\c -> case c of { [l] -> [setTrue l] ; _ -> [] })

pureAssign :: Assignment -> CNF -> Assignment
pureAssign a cnf = unionBy (\a b -> fst a == fst b) a $ 
                    let ls = nub $ concat cnf 
                    in setTrue <$> filter (\l -> not $ flipL l `elem` ls) ls
                      

dpll' :: Assignment -> CNF -> [Assignment]
dpll' a [] = [a]
dpll' _ ([]:cnf) = []
dpll' a ((l:c):cnf) = let (a', cnf') = reduceWith (setTrue l) a cnf in
                      let (a'', cnf'') = reduceWith (setFalse l) a (c:cnf)
                      in dpll' a' cnf' ++ dpll' a'' cnf''
   
apply :: Assignment -> CNF -> CNF
apply a cnf = let removeNeg = filter (\l -> evalLit l a /= Just False) <$> cnf in
              let removePos = filter (all (\l -> evalLit l a /= Just True)) removeNeg 
              in removePos 
        
reduceWith :: (String, Bool) -> Assignment -> CNF -> (Assignment, CNF)
reduceWith b a cnf = let a0   = b:a in
                     let cnf1 = apply a0 cnf in -- assign a value to a variable
                     let cnf2 = map nub cnf1 in -- filter duplicated literals in clauses  
                     let cnf3 = filter (\c -> all (\l -> not $ flipL l `elem` c) c) cnf2 in -- remove true clauses
                     let a1   = unitProp a0 cnf3 in -- assign values to unit clauses
                     let cnf4 = apply a1 cnf3 in 
                     let a2   = pureAssign a1 cnf4 in -- assign values to pure literals
                     let cnf5 = apply a2 cnf4 in 
                         (a2, if [] `elem` cnf4 then [[]] else cnf4) -- check for empty clauses

flipL (L n) = N n
flipL (N n) = L n 

solve :: Formula -> [Assignment]
solve = dpll . tseytin
