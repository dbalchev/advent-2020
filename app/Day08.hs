{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeApplications  #-}
module Day08 where
{-# LANGUAGE OverloadedStrings #-}
import           AocPrelude
import           Control.Monad (guard)
import           Prelude       ()

data InstructionType = NOP | ACC | JMP deriving (Show, Eq)


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

run :: Vector (InstructionType, Int) -> Either (Int, Int) Int
run instructions = go (0, 0) (empty @(HashSet _))
    where
        go state@(ip, register) visitedIps
            | ip `member` visitedIps = Right register
            | otherwise = result
            where
                result = case instructions !? ip of
                    Just currentInstruction -> go (execute currentInstruction state) (insert ip visitedIps)
                    Nothing -> Left (ip, register)

toggleInstruction (ACC, arg) = Nothing
toggleInstruction (NOP, arg) = Just (JMP, arg)
toggleInstruction (JMP, arg) = Just (NOP, arg)

solution input = (solution1, solution2)
    where
        instructions = fromList . map parseInstruction . lines $ input
        Right solution1 = run instructions
        (solution2:_) = do
            instructionToggleIp <- [0..length instructions - 1]
            toggledInstruction <- maybe [] singleton $ toggleInstruction (instructions ! instructionToggleIp)
            let updatedInstructions = update instructionToggleIp toggledInstruction instructions
            Left (invalidIp, register) <- return $ run updatedInstructions
            guard $ invalidIp == length instructions
            return register

-- >>> runSolution solution (TestInput "08")
-- (5,8)

-- >>> runSolution solution (RealInput "08")
-- (1949,2092)
