module Main where

valsLargerThanPreviousVal :: [Int] -> Int
valsLargerThanPreviousVal [] = 0
valsLargerThanPreviousVal [_x] = 0
valsLargerThanPreviousVal (x : y : ys)
  | y > x = 1 + valsLargerThanPreviousVal (y : ys)
  | otherwise = valsLargerThanPreviousVal (y : ys)


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

  -- Driver code for Part 1.
  -- Comment the two lines below when running Part 2.
  let resultForPartOne = valsLargerThanPreviousVal list
  print resultForPartOne

  -- Driver code for Part 2.
  -- Comment the two lines above when running Part 1.
  let resultForPartTwo = valsLargerThanPreviousVal $ alterList list
  print resultForPartTwo
