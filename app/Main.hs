module Main where

import SAT

main :: IO ()
main = do
    putStrLn "Formula: "
    line <- getLine 
    let f = read line :: Formula
    putStrLn $ show $ solve f
