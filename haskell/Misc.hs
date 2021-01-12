module Main where
import Debug.Trace
import Data.Map (Map)
import qualified Data.Map as Map

myMap = Map.fromList [("betty","555-2938"),("bonnie","452-2928"),("lucille","205-2928")]


-- Diff ':' AKA cons operator and '++' AKA list concat operator
-- https://stackoverflow.com/questions/1817865/haskell-and-differences
rev :: [Int] -> [Int]
rev [] = []
rev (x:xs) = rev xs ++ [x]
-- rev (x:xs) = (rev xs) : x -- COMPILE ERROR

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = traceShow n $ fib (n - 1) + fib (n - 2)

main = putStrLn $ "fib 4: " ++ show (fib 4)
