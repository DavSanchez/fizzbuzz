module Main (main) where

import Fizzbuzz (projectName)


main :: IO ()
main = putStrLn ("Tests for " ++ projectName)
