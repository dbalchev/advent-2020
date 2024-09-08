{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}
module Day20 where
import           AocPrelude
import           Data.Hashable (Hashable)
import           Prelude       ()


readTile (lines -> (titleLine:tileLines)) = (tileNo, fromList @(Vector (Vector Char)) . map (fromList .unpack) $ tileLines)
    where
        Just tileNoStr = stripPrefix "Tile " titleLine >>= stripSuffix ":"
        Right (tileNo :: Integer, _) = decimal tileNoStr

counter :: (Hashable a, Eq a) => [a] -> HashMap a Int
counter = foldl (flip (alter (Just . maybe 1 (+1)))) empty

-- >>> counter "abcabbcd"
-- fromList [('a',2),('b',3),('c',2),('d',1)]

findCornerIds :: (Integer -> [Vector Char]) -> [Integer] -> Vector Integer
findCornerIds idToBorders tileIds = fromList . filter isCorner $ tileIds
    where
        borderToIdCounts = counter [border | tileId <- tileIds, border <- idToBorders tileId]
        isCorner tileId = (borderFreq ! 2) == 4
            where
                borderFreq = counter . map (borderToIdCounts !) $ idToBorders tileId

idToBorders :: HashMap Integer (Vector (Vector Char)) -> Integer -> [Vector Char]
idToBorders tiles tileId = nonReversed ++ map reverse nonReversed
    where
        tile = tiles ! tileId
        n = length tile
        m = length (tile ! 0)
        nonReversed = [tile ! 0, tile ! (n - 1), (! 0) <$> tile, (! (m - 1)) <$> tile]


solve input = product cornerIds
    where
        tiles = fromList @(HashMap _ _ ) . map readTile . splitOn "\n\n" $ input
        cornerIds = findCornerIds (idToBorders tiles) (keys tiles)

-- >>> runSolution solve (TestInput "20")
-- 20899048083289

-- >>> runSolution solve (RealInput "20")
-- 18449208814679
