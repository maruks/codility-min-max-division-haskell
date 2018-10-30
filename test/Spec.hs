import Test.Hspec
import Test.Hspec.Runner (defaultConfig, hspecWith, Config(..))
import Test.QuickCheck

import Lib (minimalMaxSum, numberOfBlocks)

--import Debug.Trace

main :: IO ()
main =
  hspecWith
    defaultConfig
    { configFastFail = True
    , configPrintCpuTime = True
    }
    specs

genNumList :: Int -> Int -> Gen [Int]
genNumList maxSize maxNum = do
  n <- choose (1, maxSize)
  resize n $ listOf1 $ choose (0, maxNum)

isCorrect :: [Int] -> Int -> Int -> Bool
isCorrect xs minSum blocks =
  let blocksOk = numberOfBlocks minSum xs <= blocks
      isMin = numberOfBlocks (minSum - 1) xs > blocks
  in blocksOk && isMin -- trace ("xs = " ++ show xs ++ " , k = " ++ show blocks ++ " , minSum = " ++ show minSum) isMin

minSumProperty :: [Int] -> Property
minSumProperty xs = forAll (choose (1, length xs)) $ \k ->
    isCorrect xs (minimalMaxSum k xs) k

specs :: Spec
specs =
  describe "minimalMaxSum function" $
  do it "returns minimal large sum" $
       do minimalMaxSum 8 [2, 1, 5, 1, 2, 2, 2] `shouldBe` 5
          minimalMaxSum 7 [2, 1, 5, 1, 2, 2, 2] `shouldBe` 5
          minimalMaxSum 5 [2, 1, 5, 1, 2, 2, 2] `shouldBe` 5
          minimalMaxSum 4 [2, 1, 5, 1, 2, 2, 2] `shouldBe` 5
          minimalMaxSum 3 [2, 1, 5, 1, 2, 2, 2] `shouldBe` 6
          minimalMaxSum 2 [2, 1, 5, 1, 2, 2, 2] `shouldBe` 8
          minimalMaxSum 1 [2, 1, 5, 1, 2, 2, 2] `shouldBe` 15
          minimalMaxSum 5 [4, 7, 3, 7, 7, 5, 6] `shouldBe` 11
          minimalMaxSum 3 [6, 4, 8, 2] `shouldBe` 10
          minimalMaxSum 5 [6, 5, 1, 8, 5, 4, 1, 4, 7] `shouldBe` 11
     it "minimal large sum divides array in K blocks" $
--     verbose $
       forAll
       (genNumList 100000 10000)
       minSumProperty
