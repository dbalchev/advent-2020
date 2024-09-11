{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}
module Day20 where
import           AocPrelude
import           Control.Monad (guard)
import           Data.Hashable (Hashable (hashWithSalt))
import           Data.List     (transpose)
import           GHC.Generics  (Generic (from))
import           Prelude       ()


type Grid = Vector (Vector Char)

readTile (lines -> (titleLine:tileLines)) = (tileNo, fromList @Grid . map (fromList .unpack) $ tileLines)
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

idToBorders :: HashMap Integer Grid -> Integer -> [Vector Char]
idToBorders tiles tileId = nonReversed ++ map reverse nonReversed
    where
        tile = tiles ! tileId
        nonReversed = map ($ tile) [head, fmap head, last, fmap last]


transposeGrid :: Grid -> Grid
transposeGrid = fromList . map fromList . transpose . map toList . toList

gridVariants :: Grid -> [Grid]
gridVariants grid = map ($ grid) $ do
    f1 <- [id, reverse]
    f2 <- [id, fmap reverse]
    f3 <- [id, transposeGrid]
    return $ f1 . f2 . f3

data BorderName = BTop | BBottom | BLeft | BRight deriving(Show, Eq, Generic)

instance Hashable BorderName

allBorders = [BTop, BBottom, BLeft, BRight]

extractBorder :: BorderName -> Grid -> Vector Char
extractBorder BTop    = head
extractBorder BBottom = last
extractBorder BLeft   = fmap head
extractBorder BRight  = fmap last

makeFindTiles tiles borderName border = maybe [] toList $ borderToTile !? (borderName, border)
    where
        allVariants = do
            (tileId, tile) <- toKeyValuePairs tiles
            tileVariant <- gridVariants tile
            borderName <- allBorders
            return ((borderName, extractBorder borderName tileVariant), (tileId, tileVariant))
        borderToTile :: HashMap (BorderName, Vector Char) (Vector (Integer, Grid))
        borderToTile = collectByFirst allVariants

buildFirstRow findTile targetLength firstCornerOptions  = take targetLength <$> rowList
    where
        rowList = iterate makeNext firstCornerOptions
        makeNext currentOptions = do
            current <- currentOptions
            findTile BLeft (extractBorder BRight (snd current))

buildNextRow findTile targetLength previousRow  = take targetLength <$> rowList
    where
        rowList = drop 1 . map (map snd) . scanl makeNext [(const True, undefined)] $ previousRow
        bordersMatch left right = extractBorder BRight left == extractBorder BLeft right
        makeNext :: [(Grid -> Bool, (Integer, Grid))] -> (Integer, Grid) -> [(Grid -> Bool, (Integer, Grid))]
        makeNext leftCandidates upperTile = do
            (testFromLeft, _) <- leftCandidates
            candidate@(_, candidateTile) <- findTile BTop (extractBorder BBottom (snd upperTile))
            guard $ testFromLeft candidateTile
            return (bordersMatch candidateTile, candidate)

buildGrid findTile firstCornerOptions nRows nCols = allCandidates
    where
        firstRowOptions = buildFirstRow findTile nCols firstCornerOptions
        buildNextRowOptions = buildNextRow findTile nCols
        extendMap mapCandidates = do
            mapCandidate@(lastRow:_) <- mapCandidates
            nextRow <- buildNextRowOptions lastRow
            let newCandidate = nextRow:mapCandidate
            return newCandidate
        allCandidatesAsLists = iterate extendMap (map singleton firstRowOptions) !! (nCols - 1)
        allCandidates = do
            candidate <- allCandidatesAsLists
            let trimmedCandidate = take nRows candidate
            let vectorCandidate = fromList @(Vector (Vector _)) . map fromList $ trimmedCandidate
            return vectorCandidate

solution2 tiles topLeftId = head $ map prettyPrint grid
    where
        findTile = makeFindTiles tiles
        grid = buildGrid findTile (map (topLeftId,) $ gridVariants (tiles ! topLeftId)) 3 3
        prettyPrint = map (map fst . toList) . toList


solve input = (product cornerIds, solution2 tiles (head cornerIds))
    where
        tiles = fromList @(HashMap _ _ ) . map readTile . splitOn "\n\n" $ input
        cornerIds = findCornerIds (idToBorders tiles) (keys tiles)

-- | Day 20
-- >>> runSolution solve (TestInput "20")
-- WAS 20899048083289
-- NOW (20899048083289,[[3079],[3079,2473],[3079,3079,3079]])

-- >>> runSolution solve (RealInput "20")
-- 18449208814679
