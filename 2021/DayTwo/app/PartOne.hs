module PartOne (computeInstructions) where

type HPos = Int

type Depth = Int

type Instruction = (String, Int)

-- Takes in a list of instruction, initial horizontal position,
-- and depth and returns the final horizontal position and depth.
computeInstructions :: [Instruction] -> HPos -> Depth -> (HPos, Depth)
computeInstructions [] hPos depth = (hPos, depth)
computeInstructions (i : is) hPos depth = computeInstructions is newHPos newDepth
  where
    (newHPos, newDepth) = parseInstruction i hPos depth

    parseInstruction :: Instruction -> HPos -> Depth -> (HPos, Depth)
    parseInstruction instruction hPos depth = case instruction of
      ("forward", x) -> (hPos + x, depth)
      ("down", x) -> (hPos, depth + x)
      ("up", x) -> (hPos, depth - x)
      _ -> (0, 0)