{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeApplications  #-}
module Day08 where
{-# LANGUAGE OverloadedStrings #-}
import           AocPrelude
import           Prelude    ()

data InstructionType = NOP | ACC | JMP deriving (Show)


parseInstruction line = (opcode, arg)
    where
        [opcodeStr, argStr] = words line
        Right (arg, _) = signed decimal argStr
        opcode = case unpack opcodeStr of
            "nop" -> NOP
            "acc" -> ACC
            "jmp" -> JMP

-- >>> parseInstruction (pack "nop +0")
-- (NOP,0)

-- >>> parseInstruction (pack "acc +1")
-- (ACC,1)

-- >>> parseInstruction (pack "jmp -4")
-- (JMP,-4)

execute (NOP, _) (ip, register)   = (ip + 1, register)
execute (ACC, arg) (ip, register) = (ip + 1, register + arg)
execute (JMP, arg) (ip, register) = (ip + arg, register)

solve1 :: Vector (InstructionType, Int) -> Int
solve1 instructions = go (0, 0) (empty @(HashSet _))
    where
        go state@(ip, register) visitedIps
            | ip `member` visitedIps = register
            | otherwise = go (execute (instructions ! ip) state) (insert ip visitedIps)

solution input = solve1 instructions
    where
        instructions = fromList . map parseInstruction . lines $ input

-- >>> runSolution solution (TestInput "08")
-- 5

-- >>> runSolution solution (RealInput "08")
-- 1949
