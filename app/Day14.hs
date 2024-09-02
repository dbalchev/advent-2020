{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ViewPatterns      #-}
module Day14 where

import           AocPrelude
import           Data.Bits  (Bits (bit, complement, shiftL, testBit, (.&.), (.|.)))
import           Data.Bool  (bool)
import           Prelude    ()

data ParsedLine = SetMask Text | SetMem {address :: Int, unmaskedValue :: Int} deriving(Show)

parseLine (stripPrefix "mask = " -> Just mask) = SetMask mask
parseLine (stripPrefix "mem[" -> Just noPrefix) =  SetMem {address, unmaskedValue}
    where
        [addressStr, valueStr] = splitOn "] = " noPrefix
        Right (address, _) = decimal addressStr
        Right (unmaskedValue, _) = decimal valueStr

-- >>> parseLine "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
-- SetMask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"

-- >>> parseLine "mem[8] = 11"
-- SetMem {address = 8, unmaskedValue = 11}

data Mask a = Mask {andMask :: a, orMask :: a} deriving(Show)

parseMaskChar 'X' = Mask {andMask=0, orMask=0}
parseMaskChar '1' = Mask {andMask=0, orMask=1}
parseMaskChar '0' = Mask {andMask=1, orMask=0}

parseMaskFoldFn Mask {andMask=oldAndMask, orMask=oldOrMask} nextChar = Mask {andMask, orMask}
    where
        Mask {andMask=charAndMask, orMask=charOrMask} = parseMaskChar nextChar
        andMask = shiftL oldAndMask 1 + charAndMask
        orMask = shiftL oldOrMask 1 + charOrMask

parseMask text = Mask{orMask, andMask=complement inverseAndMask}
    where
        Mask {orMask, andMask=inverseAndMask} = foldl parseMaskFoldFn Mask {andMask=0, orMask=0} text

-- >>> parseMask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
-- Mask {andMask = -3, orMask = 64}

applyMask Mask {andMask, orMask} value = (value .|. orMask)  .&. andMask

-- >>> applyMask (parseMask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X") 11
-- 73

data DockingState = DockingState {
    mask      :: Text,
    memoryMap :: HashMap Int Int
    } deriving(Show)

maskedAddressBits '0' bit = [bit]
maskedAddressBits '1' _   = [1]
maskedAddressBits 'X' _   = [0, 1]

combineBits :: [[Int]] -> [Int]
combineBits [] = [0]
combineBits (xs:xss) = do
    rest <- combineBits xss
    x <- xs
    return $ x + shiftL rest 1

maskedAddresses maskText address = combineBits (reverse bitVariants)
    where
        bitValues = map (bool 0 1 . testBit address) [35, 34 ..0]
        bitVariants = do
            (char, bit) <- zip (unpack maskText) bitValues
            return (maskedAddressBits char bit)

-- >>> maskedAddresses "000000000000000000000000000000X1001X" 42
-- [26,27,58,59]

updateState1 DockingState {memoryMap} (SetMask mask) = DockingState {memoryMap, mask}
updateState1 DockingState {mask, memoryMap} (SetMem {address, unmaskedValue}) = DockingState{mask, memoryMap=newMemoryMap}
    where
        newMemoryMap = insert (address, applyMask (parseMask (unpack mask)) unmaskedValue) memoryMap

updateState2 DockingState {memoryMap} (SetMask mask) = DockingState {memoryMap, mask}
updateState2 DockingState {mask, memoryMap} (SetMem {address, unmaskedValue}) = DockingState{mask, memoryMap=newMemoryMap}
    where
        addresses = maskedAddresses mask address
        newMemoryMap = (fromList . map (,unmaskedValue) $ addresses) `union` memoryMap


solution input = (solution1, solution2)
    where
        parsedLines = fromList @(Vector _ ) . map parseLine . lines $ input
        initialState1 = DockingState {mask=pack $ replicate 36 'X', memoryMap=empty}
        endState1 = foldl updateState1 initialState1 parsedLines
        endState2 = foldl updateState2 initialState1 parsedLines
        [solution1, solution2] = map (sum . elems . memoryMap) [endState1, endState2]

-- >>> runSolution (fst . solution) (TestInput "14")
-- 165

-- >>> runSolution (snd . solution) (TestInput "14.2")
-- 208

-- >>> runSolution solution (RealInput "14")
-- (18630548206046,4254673508445)
