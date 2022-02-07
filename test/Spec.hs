module Main (main) where

import Fizzbuzz (Fizzbuzz (..), fizzbuzz, projectName)
import Hedgehog (Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hedgehog (testProperty)

main :: IO ()
main = do
  putStrLn ("Tests for " ++ projectName)
  defaultMain $
    testGroup
      "Properties"
      [ testProperty "Multiple of 15" prop_multipleOf15,
        testProperty "At least one number in 3 consecutive values" prop_atLeastOneNumberIn3ConsecutiveValues,
        testProperty "At least 1 Fizz in 3 consecutive values" prop_atLeast1FizzIn3ConsecutiveValues,
        testProperty "Only 1 Buzz in 5 consecutive values" prop_onlyOneBuzzIn5ConsecutiveValues,
        testProperty "At least 1 literal Buzz in 10 values" prop_atLeastOneLiteralBuzzIn10Values
      ]

prop_multipleOf15 :: Property
prop_multipleOf15 = property $ do
  n <- forAll $ (15 *) <$> Gen.int (Range.linear 1 100)
  fizzbuzz n === FizzBuzz

prop_atLeastOneNumberIn3ConsecutiveValues :: Property
prop_atLeastOneNumberIn3ConsecutiveValues = property $ do
  n <- forAll $ Gen.int (Range.linear 1 100)
  let range = [n .. n + 2]
      actual = isNumber . fizzbuzz <$> range
  or actual === True

prop_atLeast1FizzIn3ConsecutiveValues :: Property
prop_atLeast1FizzIn3ConsecutiveValues = property $ do
  n <- forAll $ Gen.int (Range.linear 1 100)
  let range = [n .. n + 2]
      actual = isFizz . fizzbuzz <$> range
  or actual === True

prop_onlyOneBuzzIn5ConsecutiveValues :: Property
prop_onlyOneBuzzIn5ConsecutiveValues = property $ do
  n <- forAll $ Gen.int (Range.linear 1 100)
  let range = [n .. n + 4]
      actual = fizzbuzz <$> range
  length (filter isBuzz actual) === 1

prop_atLeastOneLiteralBuzzIn10Values :: Property
prop_atLeastOneLiteralBuzzIn10Values = property $ do
  n <- forAll $ Gen.int (Range.linear 1 100)
  let range = [n .. n + 9]
      actual = fizzbuzz <$> range
  elem Buzz actual === True

-- prop_numbersRoundTrip = property $ do
--   n <- forAll $ Gen.int (Range.linear 0 100)
--   let range = [n..n+2]
--       actual = fizzbuzz <$> range
--       numbers = catMaybes $ readMaybe <$> actual
--   fizzbuzz n === Number (fizzbuzz (fizzbuzz n))

isNumber :: Fizzbuzz -> Bool
isNumber (Number _) = True
isNumber _ = False

isBuzz :: Fizzbuzz -> Bool
isBuzz Buzz = True
isBuzz FizzBuzz = True
isBuzz _ = False

isFizz :: Fizzbuzz -> Bool
isFizz Fizz = True
isFizz FizzBuzz = True
isFizz _ = False
