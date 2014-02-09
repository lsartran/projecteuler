-- there's an excellent library that provides us with a list of primes: let's use it
import Data.Numbers.Primes (primes)

import Data.List (sort)
import Data.List.Ordered (member)
--------------------------------------------------------------------------------
-- Problem 1
--------------------------------------------------------------------------------

problem1 :: Integer
problem1 = sum [x | x <- [1..999], ((x `mod` 3) == 0) || ((x `mod` 5) == 0)]

--------------------------------------------------------------------------------
-- Problem 2
--------------------------------------------------------------------------------

-- from http://www.haskell.org/haskellwiki/The_Fibonacci_sequence §2.2.5 With iterate
fibs :: [Integer]
fibs = map fst $ (\(a,b) -> (b,a+b)) `iterate` (0,1)

problem2 :: Integer
problem2 = sum [x | x <- takeWhile ((>=) 4000000) fibs, (x `mod` 2) == 0]

--------------------------------------------------------------------------------
-- Problem 3
--------------------------------------------------------------------------------

factors :: Integer -> [Integer]
factors n = 
    case filter (\p -> (n `mod` p) == 0) $ takeWhile ((>=) n) primes of
        [] -> []
        p:_ -> p:factors (n `div` p)

largestPrimeFactor n =
    last $ factors n

problem3 = largestPrimeFactor 600851475143

--------------------------------------------------------------------------------
-- Problem 4
--------------------------------------------------------------------------------

-- DONE, TO BE WRITTEN

--------------------------------------------------------------------------------
-- Problem 5
--------------------------------------------------------------------------------

problem5 = foldl1 lcm [1..20]

--------------------------------------------------------------------------------
-- Problem 6
--------------------------------------------------------------------------------

-- Pen & paper

--------------------------------------------------------------------------------
-- Problem 7
--------------------------------------------------------------------------------

problem7 = primes !! 10000

--------------------------------------------------------------------------------
-- Problem 9
--------------------------------------------------------------------------------

problem9 :: Integer
problem9 = head [a*b*(1000-a-b) | a<-[1..1000], b<-[a..1000], (a^2)+(b^2)==(1000-a-b)^2]

--------------------------------------------------------------------------------
-- Problem 10
--------------------------------------------------------------------------------

problem10 = sum $ takeWhile ((>=) 2000000) primes

--------------------------------------------------------------------------------
-- Problem 16
--------------------------------------------------------------------------------

sumDigits :: Integer -> Integer
sumDigits n
    | n < 0 = error "Non-negative integers only"
    | n <= 9 = n
    | otherwise = (rem n 10) + (sumDigits $ div n 10)

problem16 = sumDigits $ 2^1000

--------------------------------------------------------------------------------
-- Problem 20
--------------------------------------------------------------------------------

fact :: Integer -> Integer
fact n = product [1..n]

problem20 = sumDigits $ fact 100

--------------------------------------------------------------------------------
-- Problem 25
--------------------------------------------------------------------------------

-- the explicit formula for F_n yields

problem25 = floor $ (999*(log 10) + (log $ sqrt 5)) / (log $ ((1 + (sqrt 5))/2)) + 0.5

--------------------------------------------------------------------------------
-- Problem 49
--------------------------------------------------------------------------------

problem49 =
    let fourDigitsPrimes = takeWhile ((>=) 9999) $ dropWhile ((>) 1000) primes
        digits n
            | n < 0 = error "Non-negative integers only"
            | n <= 9 = [n]
            | otherwise = (rem n 10) : (digits $ div n 10)
        sortedDigits n = id $! sort $ digits n
        eligibleSet = filter (\(a,b,c) -> (sortedDigits b) == (sortedDigits c)) $ filter (\(a,b,c) -> (sortedDigits a) == (sortedDigits b)) $ [(a,b,2*b-a) | a <- fourDigitsPrimes, b <- fourDigitsPrimes, b > a, (2*b-a) `member` fourDigitsPrimes]
        (a,b,c) = eligibleSet !! 1 in
        c+10000*(b+10000*a)

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main = do
    putStrLn $ show problem1
    putStrLn $ show problem2
    putStrLn $ show problem3
    putStrLn $ show problem5
    putStrLn $ show problem7
    putStrLn $ show problem9
    putStrLn $ show problem10
    putStrLn $ show problem16
    putStrLn $ show problem20
    putStrLn $ show problem25
