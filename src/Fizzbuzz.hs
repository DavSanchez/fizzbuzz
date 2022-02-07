{-# LANGUAGE DerivingStrategies #-}
-- |
-- Copyright: (c) 2022 David Sánchez
-- SPDX-License-Identifier: MIT
-- Maintainer: David Sánchez <david.sanchez.lt@gmail.com>
--
-- See README for more info
module Fizzbuzz
  ( fizzbuzz,
    projectName,
    Fizzbuzz(..),
  )
where

data Fizzbuzz = Fizz | Buzz | FizzBuzz | Number Int
  deriving stock (Show, Eq)

projectName :: String
projectName = "fizzbuzz"

fizzbuzz :: (Integral a) => a -> Fizzbuzz
fizzbuzz n
  | n `mod` 15 == 0 = FizzBuzz
  | n `mod` 5 == 0 = Buzz
  | n `mod` 3 == 0 = Fizz
  | otherwise = Number (fromIntegral n)
