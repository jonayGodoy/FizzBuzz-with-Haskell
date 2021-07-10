import Test.Hspec
import Test.QuickCheck

print1 = putStrLn "sss"

print2 = (putStrLn "2222")

print3 = putStrLn $ "333"

isFizz :: Integer -> Bool
isFizz x = x `mod` 3 == 0

fizzGenerator :: Integer -> String
fizzGenerator x
 | isFizz x = "Fizz"
 | otherwise = show x


main :: IO ()
main = hspec $ do
 describe "Tests" $ do
  it "green test" $ do
   True == True
 describe "FizzBuzz" $ do
  it "execute unfizzbuzzable number" $ do
   fizzGenerator 1 == "1"
  it "execute fizzable number" $ do
   fizzGenerator 3 == "Fizz"

