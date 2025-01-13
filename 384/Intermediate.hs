import           Control.Monad
import           Data.Array.ST
import           Data.Array.Unboxed

sieveUA :: Integer -> UArray Integer Bool
sieveUA top = runSTUArray $ do
  let m = (top - 1) `div` 2
      r = floor . sqrt $ fromIntegral top + 1
  sieve <- newArray (1, m) True
  forM_ [1 .. r `div` 2] $ \i -> do
    isPrime <- readArray sieve i
    when isPrime $ forM_ [2 * i * (i + 1), 2 * i * (i + 2) + 1 .. m] $ \j ->
      writeArray sieve j False
  return sieve

primesTo :: Integer -> [Integer]
primesTo n = 2 : [ i * 2 + 1 | (i, True) <- assocs $ sieveUA n ]

necklaces :: Integer -> Integer -> Integer
necklaces k n =
  sum
      [ phi primes a * k ^ b
      | a <- [1 .. n]
      , n `mod` a == 0
      , let b = n `div` a
      ]
    `div` n
  where primes = primesTo n

phi :: [Integer] -> Integer -> Integer
phi primes a = a * product (map (subtract 1) pfactors) `div` product pfactors
  where pfactors = filter (\x -> a `mod` x == 0) $ takeWhile (<= a) primes

sqrt' :: Integer -> Integer
sqrt' n = round (sqrt $ fromIntegral n)

main :: IO ()
main = do
  print $ necklaces 2 12
  print $ necklaces 3 7
  print $ necklaces 9 4
  print $ necklaces 21 3
  print $ necklaces 99 2
  print $ necklaces 3 90
  print $ necklaces 123 18
  print $ necklaces 1234567 6
  print $ necklaces 12345678910 3
