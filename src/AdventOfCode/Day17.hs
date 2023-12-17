module AdventOfCode.Day17 (day17, part1, part2) where

import Data.Char (digitToInt)
import Data.Graph.Inductive.Graph (Graph (mkGraph), LEdge, LNode)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query (spLength)
import Data.Maybe (catMaybes, fromJust)
import Data.Tuple (swap)
import Lens.Micro (both, ix, over, (^?))

day17 :: IO ()
day17 = do
  input <- readFile "src/Data/Day17.txt"
  putStr "Day 17 part 1: "
  print . part1 $ input
  putStr "Day 17 part 2: "
  print . part2 $ input

part1 :: String -> Int
part1 = shortestPath 0 3 . inputParser

part2 :: String -> Int
part2 = shortestPath 3 10 . inputParser

type HeatMap = [[Int]]

type HeatLoss = Int

data HeatNode
  = HNode {_nodeNum :: Int}
  | VNode {_nodeNum :: Int}
  deriving (Show, Eq)

type HeatEdge = (HeatNode, HeatNode, HeatLoss)

inputParser :: String -> HeatMap
inputParser = map (map digitToInt) . init . lines

nodeToPos :: HeatMap -> HeatNode -> (Int, Int)
nodeToPos hmap node = swap $ _nodeNum node `divMod` xDim
  where
    xDim = length . head $ hmap

posToNodeNum :: HeatMap -> (Int, Int) -> Int
posToNodeNum hmap (x, y) = x + y * xDim
  where
    xDim = length . head $ hmap

heatLoss :: HeatMap -> HeatNode -> Maybe HeatLoss
heatLoss hmap node = hmap ^? ix y . ix x
  where
    (x, y) = nodeToPos hmap node

outEdges :: HeatMap -> Int -> Int -> HeatNode -> [HeatEdge]
outEdges hmap skip amount node = zip3 (repeat node) (dropLeft left ++ dropRight right) (lHeatLoss ++ rHeatLoss)
  where
    (left, right) = adjacentNodes hmap node amount
    lHeatLoss = dropLeft . init . scanr (\n a -> a + (fromJust . heatLoss hmap $ n)) 0 $ left
    rHeatLoss = dropRight . tail . scanl (\a n -> a + (fromJust . heatLoss hmap $ n)) 0 $ right
    dropLeft = reverse . drop skip . reverse
    dropRight = drop skip

adjacentNodes :: HeatMap -> HeatNode -> Int -> ([HeatNode], [HeatNode])
adjacentNodes hmap node amount = over both (map (toggleNode . posToNodeNum hmap)) (adjacentPos hmap node amount)
  where
    toggleNode = case node of
      (HNode _) -> VNode
      (VNode _) -> HNode

adjacentPos :: HeatMap -> HeatNode -> Int -> ([(Int, Int)], [(Int, Int)])
adjacentPos hmap node amount = over both (filter valid . map (nodeAdd (nodeToPos hmap node))) (leftRange, rightRange)
  where
    leftRange = reverse . map negate $ [1 .. amount]
    rightRange = [1 .. amount]
    nodeAdd = case node of
      (HNode _) -> (\(x, y) n -> (x + n, y))
      (VNode _) -> (\(x, y) n -> (x, y + n))
    valid (x, y) = 0 <= x && x < (length . head $ hmap) && 0 <= y && y < length hmap

generateHeatNodes :: HeatMap -> [HeatNode]
generateHeatNodes hmap = concatMap (\n -> [HNode n, VNode n]) [0 .. maxNodeNum hmap]

convertHeatNode :: HeatMap -> HeatNode -> Int
convertHeatNode _ (HNode n) = n
convertHeatNode hmap (VNode n) = n + maxNodeNum hmap + 1

maxNodeNum :: HeatMap -> Int
maxNodeNum hmap = length hmap * (length . head $ hmap) - 1

heatNodeToLNode :: HeatMap -> HeatNode -> LNode String
heatNodeToLNode hmap node = (convertHeatNode hmap node, label)
  where
    label = case node of
      (HNode _) -> "H"
      (VNode _) -> "V"

heatEdgeToLEdge :: HeatMap -> HeatEdge -> LEdge Int
heatEdgeToLEdge hmap (n1, n2, hl) = (convertHeatNode hmap n1, convertHeatNode hmap n2, hl)

generateGraph :: HeatMap -> Int -> Int -> Gr String Int
generateGraph hmap start end = mkGraph lnodes ledges
  where
    hnodes = generateHeatNodes hmap
    hedges = concatMap (outEdges hmap start end) hnodes
    lnodes = map (heatNodeToLNode hmap) hnodes
    ledges = map (heatEdgeToLEdge hmap) hedges

shortestPath :: Int -> Int -> HeatMap -> Int
shortestPath start end hmap = minimum . catMaybes $ [spLength startNode endNode graph | startNode <- startNodes, endNode <- endNodes]
  where
    graph = generateGraph hmap start end
    startNodes = map (convertHeatNode hmap) [HNode 0, VNode 0]
    endNodes = map (convertHeatNode hmap) [HNode (maxNodeNum hmap), VNode (maxNodeNum hmap)]
