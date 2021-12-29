module PartTwo (computeInstructions) where

type HPos = Int

type Depth = Int

type Aim = Int

type Instruction = (String, Int)

-- Takes in a list of instruction, initial horizontal position, aim, 
-- and depth and returns the final horizontal position and depth.
computeInstructions :: [Instruction] -> HPos -> Depth -> Aim -> (HPos, Depth)
computeInstructions [] hPos depth _aim = (hPos, depth)
computeInstructions (i : is) hPos depth aim = computeInstructions is newHPos newDepth newAim
  where
    (newHPos, newDepth, newAim) = parseInstruction i hPos depth aim

    parseInstruction :: Instruction -> HPos -> Depth -> Aim -> (HPos, Depth, Aim)
    parseInstruction instruction hPos depth aim = case instruction of
      ("forward", x) -> (hPos + x, depth + (aim * x), aim)
      ("down", x) -> (hPos, depth, aim + x)
      ("up", x) -> (hPos, depth, aim - x)
      _ -> (0, 0, 0)
