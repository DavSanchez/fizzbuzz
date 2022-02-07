module Main (main) where

import Fizzbuzz (projectName)


main :: IO ()
main = putStrLn ("Executable for " ++ projectName)
