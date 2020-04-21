module Main where

import Control.Monad

import Unification

main :: IO ()
main = do
    putStrLn "Term#1: "
    line1 <- getLine 
    unless (line1 == "q") $ do
        putStrLn "Term#2: "
        line2 <- getLine 
        unless (line2 == "q") $ do
            let (f, g) = (read line1 :: Term, read line2 :: Term)
            putStrLn $ show $ unify [] f g
            main
