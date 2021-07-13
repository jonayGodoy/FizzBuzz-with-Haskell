import Test.Hspec
import Test.QuickCheck

-- First Solution

isFizz :: Integer -> Bool
isFizz x = x `mod` 3 == 0

isBuzz :: Integer -> Bool
isBuzz x = x `mod` 5 == 0

fizzGenerator :: Integer -> FizzBuzzResult
fizzGenerator x
 | (isFizz x) && (isBuzz x) = FizzBuzz
 | isFizz x = Fizz
 | isBuzz x = Buzz
 | otherwise = JustNumber x

data FizzBuzzResult = FizzBuzz | Fizz | Buzz | JustNumber Integer deriving (Eq, Show)

-- Second Solution
-- This solution doesn't have coupling by execution
data FizzBuzzable' = FizzBuzz' | Fizz' Integer | Buzz' Integer | JustNumber' Integer deriving (Eq, Show)

applyFizz :: FizzBuzzable' -> FizzBuzzable'
applyFizz (Buzz' x) = if x `mod` 3 == 0 then FizzBuzz' else (Buzz' x)
applyFizz (JustNumber' x) = if x `mod` 3 == 0 then (Fizz' x) else JustNumber' x
applyFizz FizzBuzz' = FizzBuzz'

applyBuzz :: FizzBuzzable' -> FizzBuzzable'
applyBuzz (Fizz' x) = if x `mod` 5 == 0 then FizzBuzz' else (Fizz' x)
applyBuzz (JustNumber' x) = if x `mod` 5 == 0 then (Buzz' x) else JustNumber' x
applyBuzz FizzBuzz' = FizzBuzz'

fizzGenerator' :: Integer -> FizzBuzzable'
fizzGenerator' x = applyFizz . applyBuzz $ JustNumber' x

main :: IO ()
main = hspec $ do
 describe "Tests" $ do
  it "green test" $ do
   True == True
 describe "FizzBuzz" $ do
  it "execute unfizzbuzzable number" $ do
   fizzGenerator 1 == JustNumber 1
  it "execute fizzable number" $ do
   fizzGenerator 3 == Fizz
  it "execute buzzable number" $ do
   fizzGenerator 5 == Buzz
  it "execute fizzbuzzable number" $ do
   fizzGenerator 15 == FizzBuzz
 describe "FizzBuzz2" $ do
  it "execute return just number" $ do
   fizzGenerator' 1 == JustNumber' 1
  it "execute return fizz" $ do
   fizzGenerator' 3 == Fizz' 3
  it "execute return buzz" $ do
   fizzGenerator' 5 == Buzz' 5
  it "execute return buzz" $ do
   fizzGenerator' 15 == FizzBuzz' 
