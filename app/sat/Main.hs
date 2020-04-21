module Main where

import Control.Monad

import SAT

main :: IO ()
main = do
    putStrLn "Formula: "
    line <- getLine 
    unless (line == "q") $ do
        let f = read line :: Formula
        putStrLn $ show $ filter ((flip elem) (vars f) . fst) <$> solve f
        main
