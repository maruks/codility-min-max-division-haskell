module Lib
  ( numberOfBlocks
  , minimalMaxSum
  ) where

numberOfBlocks :: Int -> [Int] -> Int
numberOfBlocks maxSum xs
  | maximum xs > maxSum = maxBound
  | otherwise =
    let (s, b) =
          foldl
            (\(sum, result) e ->
                if sum + e > maxSum
                  then (e, result + 1)
                  else (sum + e, result))
            (0, 0)
            xs
    in (b +
        (if s > 0
           then 1
           else 0))

minimalMaxSum' :: Int -> Int -> Int -> Int -> [Int] -> Int
minimalMaxSum' minSum maxSum blocksTarget result xs
  | minSum > maxSum = result
  | otherwise =
    let sum = (minSum + maxSum) `div` 2
        blocks = numberOfBlocks sum xs
    in let (nextMin, nextMax, nextResult) =
             if blocks <= blocksTarget
               then (minSum, sum - 1, sum)
               else (sum + 1, maxSum, result)
       in minimalMaxSum' nextMin nextMax blocksTarget nextResult xs

minimalMaxSum :: Int -> [Int] -> Int
minimalMaxSum blocks xs =
  let minSum = maximum xs
      maxSum = sum xs
  in minimalMaxSum' minSum maxSum blocks maxSum xs
