module Main where

import Lib (minimalMaxSum)

import Data.Text (Text)
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as R

import Control.Monad (forM_)
import Data.Array.MArray (newArray, writeArray, getElems)
import Data.Array.IO (IOUArray)

decimal :: Text -> Int
decimal text =
  case R.decimal text of
    Left e -> error e
    Right (i, _) -> i

readInput :: IO [Int]
readInput = do
  size <- decimal <$> TIO.getLine
  ioArray <- newArray (1, size) 0 :: IO (IOUArray Int Int)
  forM_ [1 .. size] $
    \idx -> do
      i <- decimal <$> TIO.getLine
      writeArray ioArray idx i
  getElems ioArray


main :: IO ()
main = do
  xs <- readInput
  print $ minimalMaxSum (head xs) (tail xs)
