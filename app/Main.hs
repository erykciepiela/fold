module Main where

import Control.Monad

cases :: [([Int], [[Int]])]
cases = [
    ([], []),
    ([1], [[1]]),
    ([1, 2], [[1], [1, 2], [2]]),
    ([1, 2, 3], [[1], [1, 2], [1, 2, 3], [2, 3], [3]]),
    ([1, 2, 3, 4], [[1], [1, 2], [1, 2, 3], [1, 2, 3, 4], [2, 3, 4], [3, 4], [4]])
    ]

prefixSuffixFold :: [a] -> [[a]]
prefixSuffixFold = foldl acc init
    where
        init :: [[a]]
        init = []
        acc :: [[a]] -> a -> [[a]]
        acc [] a = [[a]]
        acc as a = let 
            l = length as `div` 2
            in take (l + 1) as <> ((<> [a]) <$> drop l as) <> [[a]]

main :: IO ()
main = forM_ cases $ \(i, o) -> do
    print i
    print o
    print (prefixSuffixFold i)
    print (prefixSuffixFold i == o)
