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
import           Data.Bool     (bool)
import           Data.Hashable (Hashable (hashWithSalt))
import           Data.List     (intercalate, intersperse, transpose)
import           GHC.Generics  (Generic (from))
import           Prelude       ()


type Grid = Vector (Vector Char)

readTile (lines -> (titleLine:tileLines)) = (tileNo, fromList @Grid . map (fromList .unpack) $ tileLines)
    where
        Just tileNoStr = stripPrefix "Tile " titleLine >>= stripSuffix ":"
        Right (tileNo :: Integer, _) = decimal tileNoStr

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

buildRowGeneric tileConstraints findTile targetLength firstCornerOptions = fmap reverse (fst $ rowList !! (targetLength - 1))
    where
        rowList = iterate makeNext (singleton <$> firstCornerOptions, tileConstraints)
        makeNext (currentOptions, currentConstraint:restConstaints) = (nextOptions, restConstaints)
            where
                nextOptions = do
                    currentRow@(currentLast@(currentId, _):_) <- currentOptions
                    next@(nextId, _) <- findTile BLeft (extractBorder BRight (snd currentLast))
                    guard $ currentConstraint next
                    guard $ currentId /= nextId
                    return (next:currentRow)

buildFirstRow = buildRowGeneric (repeat (const True))
buildNextRow findTile targetLength (firstOfPrevious:restOfPrevious) = buildRowGeneric tileConstraints findTile targetLength firstCornerOptions
    where
        bordersMatch (topId, top) (bottomId, bottom) = topId /= bottomId && extractBorder BBottom top == extractBorder BTop bottom
        tileConstraints = map bordersMatch restOfPrevious
        firstCornerOptions = findTile BTop (extractBorder BBottom (snd firstOfPrevious))
-- buildNextRow findTile targetLength previousRow = undefined
--     where
--         makeNext (currentOptions, )
-- buildNextRow findTile targetLength previousRow  = take targetLength <$> rowList
--     where
--         rowList = transpose . drop 1 . map (map snd) . scanl makeNext [(const True, undefined)] $ previousRow
--         bordersMatch (leftId, left) (rightId, right) = leftId /= rightId && extractBorder BRight left == extractBorder BLeft right
--         makeNext :: [((Integer, Grid) -> Bool, (Integer, Grid))] -> (Integer, Grid) -> [((Integer, Grid) -> Bool, (Integer, Grid))]
--         makeNext leftCandidates upperTile = do
--             (testFromLeft, _) <- leftCandidates
--             candidate <- findTile BTop (extractBorder BBottom (snd upperTile))
--             guard $ testFromLeft candidate
--             return (bordersMatch candidate, candidate)

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
            let vectorCandidate = fromList @(Vector (Vector _)) . map fromList . reverse $ trimmedCandidate
            return vectorCandidate


dropBorders :: Grid -> Grid
dropBorders = fromList . map sliceVector . toList . sliceVector
    where
        sliceVector v = slice 1 (length v - 2) v

-- | dropBorders test
-- >>> map toList . toList . dropBorders . fromList . map fromList $ ["1234", "5678", "90AB", "CDEF"]
-- ["67","0A"]

concatCols :: [Grid] -> Grid
concatCols cols = fromList . map mconcat . transpose $ gridRows
    where
        gridRows = map toList cols

-- | concatCols test
-- >>> map toList . toList . concatCols . map (fromList . map fromList) $ [["AB", "CD"], ["EF", "GH"]]
-- ["ABEF","CDGH"]


concatRows :: [Grid] -> Grid
concatRows = fromList . concatMap toList

-- | concatRows test
-- >>> map toList . toList . concatRows . map (fromList . map fromList) $ [["AB", "CD"], ["EF", "GH"]]
-- ["AB","CD","EF","GH"]

reconstructMap :: Vector (Vector (a, Grid)) -> Grid
reconstructMap = concatRows . map (((concatCols . map dropBorders) . map snd) . toList) . toList

seaMonsterDiagram = [
    "                  # ",
    "#    ##    ##    ###",
    " #  #  #  #  #  #   "
    ]
seaMonsterIndices = do
    (i, row) <- zip [0..] seaMonsterDiagram
    (j, char) <- zip [0..] row
    guard $ char == '#'
    return (i, j)

seaMonsterRows = 1 + (maximum . map fst $ seaMonsterIndices)
seaMonsterCols = 1 + (maximum . map snd $ seaMonsterIndices)
-- >>> (seaMonsterRows, seaMonsterCols)
-- (3,20)
hasSeaMonserAtIndex :: Grid -> (Int, Int) -> Bool
hasSeaMonserAtIndex grid (i, j)
    | (i + seaMonsterRows > nRows) || (j + seaMonsterCols > nCols) = False
    | otherwise = all hasRequiredChar seaMonsterIndices
    where
        nRows = length grid
        nCols = length (grid ! 0)
        hasRequiredChar (si, sj) = grid ! (si + i) ! (sj + j) == '#'

countSeaMonsters grid = sum . map (bool 0 1) $ monsterMask
    where
        nRows = length grid
        nCols = length (grid ! 0)
        monsterMask = do
            i <- [0..(nRows - seaMonsterRows)]
            j <- [0..(nCols - seaMonsterCols)]
            return $ hasSeaMonserAtIndex grid (i, j)


solution2 sideSize tiles topLeftId = roughness
    where
        findTile = makeFindTiles tiles
        firstCornerOptions = map (topLeftId,) $ gridVariants (tiles ! topLeftId)
        (foo:_) = do
            firstRow <- buildFirstRow findTile sideSize firstCornerOptions
            secondRow <- buildNextRow findTile sideSize firstRow
            return [firstRow, secondRow]
        (grid:_) = buildGrid findTile (map (topLeftId,) $ gridVariants (tiles ! topLeftId)) sideSize sideSize
        reconstructedMap = reconstructMap grid
        prettyPrinted = intercalate "\n" . map toList . toList $ reconstructedMap
        [nMonsters] = filter (/= 0) . map countSeaMonsters . gridVariants $ reconstructedMap
        hashes = sum . map (bool 0 1 . (== '#')) . concatMap toList . toList $ reconstructedMap
        roughness = hashes - nMonsters * length seaMonsterIndices


solve sideSize input = (product cornerIds, solution2 sideSize tiles (head cornerIds))
    where
        tiles = fromList @(HashMap _ _ ) . map readTile . splitOn "\n\n" $ input
        cornerIds = findCornerIds (idToBorders tiles) (keys tiles)

-- | Day 20
-- >>> runSolution (solve 3) (TestInput "20")
-- (20899048083289,273)

-- >>> runSolution (solve 12) (RealInput "20")
-- (18449208814679,1559)
