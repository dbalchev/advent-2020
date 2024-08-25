{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Day07 where
import           AocPrelude
import           Data.Foldable (Foldable (toList))
import           Prelude       ()

data Regulation = Regulation {
    bagType         :: Text,
    possibleContent :: Vector (Int, Text)
    } deriving (Show)


parseContent line = (count, unwords . init $  description)
    where
        (countStr:description) = words line
        Right (count, _) = decimal countStr

-- >>> parseContent "1 bright white bag"
-- (1,"bright white")

parseRegulation line = Regulation {bagType, possibleContent}
    where
        [bagType, contentStr] = splitOn " bags contain " line
        possibleContent = if
            contentStr == "no other bags."
            then empty @(Vector _)
            else fromList . map parseContent . splitOn ", " $ contentStr

-- >>> parseRegulation "light red bags contain 1 bright white bag, 2 muted yellow bags."
-- Regulation {bagType = "light red", possibleContent = RootNode {vecSize = 2, vecShift = 5, vecTail = [(2,"muted yellow"),(1,"bright white")], intVecPtrs = []}}

-- >>> parseRegulation "faded blue bags contain no other bags."
-- Regulation {bagType = "faded blue", possibleContent = RootNode {vecSize = 0, vecShift = 5, vecTail = [], intVecPtrs = []}}

data Vertex = Vertex {
    name     :: Text,
    children :: Vector Text
} deriving(Show)

regulationToVertex Regulation {bagType, possibleContent}
    = Vertex {name=bagType, children=fromList . map snd .toList $ possibleContent}

reverseGraph vertices = map pairToVertex . toKeyValuePairs $ go fromToList empty
    where
        fromToList = do
            Vertex {name=fromVertex, children} <- toList vertices
            toVertex <- toList children
            return (fromVertex, toVertex)
        go [] acc                            = acc
        go ((fromVertex, toVertex):rest) acc = go rest $ alter (add fromVertex) toVertex acc
        add toVertex Nothing         = Just (singleton toVertex)
        add toVertex (Just vertices) = Just (snoc vertices toVertex)
        pairToVertex (name, children) = Vertex {name, children}

-- >>> reverseGraph [Vertex {name="foo", children=fromList ["bar", "baz"]}, Vertex {name="foobar", children=fromList ["bar"]}]
-- [Vertex {name = "baz", children = RootNode {vecSize = 1, vecShift = 5, vecTail = ["foo"], intVecPtrs = []}},Vertex {name = "bar", children = RootNode {vecSize = 2, vecShift = 5, vecTail = ["foobar","foo"], intVecPtrs = []}}]

collectDescendents childrenOfVertex [] acc = acc
collectDescendents childrenOfVertex (x:xs) acc
    | x `member` acc = collectDescendents childrenOfVertex xs acc
    | otherwise = collectDescendents childrenOfVertex (childrenOfVertex x ++ xs) (insert x acc)

solve input = length descendents - 1
    where
        regulations = fromList @(Vector _) . map parseRegulation . lines $ input
        simpleGraph = fromList @(Vector _) . map regulationToVertex . toList $ regulations
        reversedGraph = fromList @(Vector _) $ reverseGraph  simpleGraph
        childrenOfVertex = fromList @(HashMap _ _) . map (\Vertex {name, children} -> (name, children)) . toList $ reversedGraph
        descendents :: HashSet Text
        descendents = collectDescendents (maybe [] toList .(childrenOfVertex !?)) ["shiny gold"] empty

-- >>> runSolution solve (TestInput "07")
-- 4

-- >>> runSolution solve (RealInput "07")
-- 265
