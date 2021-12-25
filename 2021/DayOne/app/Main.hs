module Main where

valsLargerThanPreviousVals :: [Int] -> Int
valsLargerThanPreviousVals [] = 0
valsLargerThanPreviousVals [_x] = 0
valsLargerThanPreviousVals (x : y : ys)
  | y > x = 1 + valsLargerThanPreviousVals (y : ys)
  | otherwise = valsLargerThanPreviousVals (y : ys)


-- Create a new list, which at each index contains the sum of consecutive triplets.
alterList :: [Int] -> [Int]
alterList [] = []
alterList [_x] = []
alterList [_x, _y] = []
alterList (x : y : z : zs) = (x + y + z) : alterList (y : z : zs)

main :: IO ()
main = do
  fileContents <- getContents
  let list = (\x -> read x :: Int) <$> words fileContents

  let resultForPartOne = valsLargerThanPreviousVals list
  print resultForPartOne

  let resultForPartTwo = valsLargerThanPreviousVals $ alterList list
  print resultForPartTwo
