module Main where

import PartOne (computeInstructions)
import PartTwo (computeInstructions)

type Instruction = (String, Int)

-- Convert a list of two elements to a pair
makePair :: [String] -> Instruction
makePair [x, y] = (x, read y :: Int)
makePair _ = ("", 0)

main :: IO ()
main = do
  fileContents <- getContents
  let contentList = lines fileContents
  let instructions = makePair . words <$> contentList

  let (hPos, depth) = PartOne.computeInstructions instructions 0 0
  let resultForPartOne = hPos * depth
  print resultForPartOne

  let (hPos, depth) = PartTwo.computeInstructions instructions 0 0 0
  let resultForPartTwo = hPos * depth
  print resultForPartTwo