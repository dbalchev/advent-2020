{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ViewPatterns      #-}
module Day14 where

import           AocPrelude
import           Data.Bits  (Bits (complement, shiftL, (.&.), (.|.)))
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
    mask      :: Mask Int,
    memoryMap :: HashMap Int Int
    } deriving(Show)

updateState DockingState {memoryMap} (SetMask maskText) = DockingState {memoryMap, mask=parseMask $ unpack maskText}
updateState DockingState {mask, memoryMap} (SetMem {address, unmaskedValue}) = DockingState{mask, memoryMap=newMemoryMap}
    where
        newMemoryMap = insert (address, applyMask mask unmaskedValue) memoryMap

solution input = sum . elems . memoryMap $ endState
    where
        parsedLines = fromList @(Vector _ ) . map parseLine . lines $ input
        initialMask = Mask {orMask=0, andMask=complement 0}
        initialState = DockingState {mask=initialMask, memoryMap=empty}
        endState = foldl updateState initialState parsedLines

-- >>> runSolution solution (TestInput "14")
-- 165

-- >>> runSolution solution (RealInput "14")
-- 18630548206046
