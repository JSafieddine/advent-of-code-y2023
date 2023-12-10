module AdventOfCode.Day10 (day10, part1, part2) where

import Control.Monad.State (MonadState (get), State, evalState, put)
import Data.Functor (($>))
import Data.Graph (Vertex)
import Data.List (intersect, transpose)
import Data.Maybe (fromJust)
import Data.Tuple (swap)
import Data.Void (Void)
import Lens.Micro (each, ix, over, (&), (.~))
import Text.Megaparsec (MonadParsec (eof), Parsec, parse, some, (<|>))
import Text.Megaparsec.Char (char, eol)

day10 :: IO ()
day10 = do
  input <- readFile "src/Data/Day10.txt"
  putStr "Day 10 part 1: "
  print . part1 $ input
  putStr "Day 10 part 2: "
  print . part2 $ input

part1 :: String -> Int
part1 input = (length . getLoop $ pm) `div` 2
  where
    pm = case parse inputParser "" input of
      (Right result) -> result
      (Left _) -> error "Can't parse Input!"

part2 :: String -> Int
part2 input = length . scanInLoop $ pm
  where
    pm = case parse inputParser "" input of
      (Right result) -> result
      (Left _) -> error "Can't parse Input!"

type Parser = Parsec Void String

data Pipe = NSPipe | WEPipe | NEPipe | NWPipe | SWPipe | SEPipe | Ground | Start deriving (Show, Eq)

type PipeMap = [[Pipe]]

type PipePos = (Int, Int)

pipeParser :: Parser Pipe
pipeParser =
  char '|'
    $> NSPipe
      <|> char '-'
    $> WEPipe
      <|> char 'L'
    $> NEPipe
      <|> char 'J'
    $> NWPipe
      <|> char '7'
    $> SWPipe
      <|> char 'F'
    $> SEPipe
      <|> char '.'
    $> Ground
      <|> char 'S'
    $> Start

lineParser :: Parser [Pipe]
lineParser = some pipeParser <* eol

inputParser :: Parser PipeMap
inputParser = some lineParser <* eol <* eof

getAdjacentPipePos :: PipeMap -> Vertex -> (Maybe PipePos, Maybe PipePos, Maybe PipePos, Maybe PipePos)
getAdjacentPipePos pm v = (maybePos (vx - 1, vy), maybePos (vx, vy - 1), maybePos (vx + 1, vy), maybePos (vx, vy + 1))
  where
    (vx, vy) = vertexToPipeMapPos pm v
    maybePos pos = if posInBounds pm pos then Just pos else Nothing

getAdjacentPipes :: PipeMap -> Vertex -> (Maybe Pipe, Maybe Pipe, Maybe Pipe, Maybe Pipe)
getAdjacentPipes pm v = over each maybeGetPipe (getAdjacentPipePos pm v)
  where
    maybeGetPipe mp = getPipe pm <$> mp

vertexToPipeMapPos :: PipeMap -> Vertex -> PipePos
vertexToPipeMapPos pm v = swap $ v `divMod` pmLength
  where
    pmLength = length . head $ pm

pipeMapPosToVertex :: PipeMap -> PipePos -> Vertex
pipeMapPosToVertex pm (vx, vy) = vx + vy * pmLength
  where
    pmLength = length . head $ pm

getVertices :: PipeMap -> [Vertex]
getVertices pm = [0 .. length pm * (length . head $ pm) - 1]

posInBounds :: PipeMap -> PipePos -> Bool
posInBounds pm (vx, vy)
  | vx < 0 || vy < 0 = False
  | vx >= pmLength || vy >= pmHight = False
  | otherwise = True
  where
    pmLength = length . head $ pm
    pmHight = length pm

getPipe :: PipeMap -> PipePos -> Pipe
getPipe pm (vx, vy) = pm !! vy !! vx

getStartPos :: PipeMap -> PipePos
getStartPos pm = head . filter ((== Start) . getPipe pm) . map (vertexToPipeMapPos pm) . getVertices $ pm

replaceStartPipe :: PipeMap -> PipeMap
replaceStartPipe pm
  | vConnected mAbove (Just NSPipe) && vConnected (Just NSPipe) mBelow = adjustStartPipe NSPipe
  | hConnected mLeft (Just WEPipe) && hConnected (Just WEPipe) mRight = adjustStartPipe WEPipe
  | vConnected mAbove (Just NEPipe) && hConnected (Just NEPipe) mRight = adjustStartPipe NEPipe
  | vConnected mAbove (Just NWPipe) && hConnected mLeft (Just NWPipe) = adjustStartPipe NWPipe
  | vConnected (Just SWPipe) mBelow && hConnected mLeft (Just SWPipe) = adjustStartPipe SWPipe
  | vConnected (Just SEPipe) mBelow && hConnected (Just SEPipe) mRight = adjustStartPipe SEPipe
  | otherwise = error "Start is not connected to exactly two pipes!"
  where
    startPos@(x, y) = getStartPos pm
    (mLeft, mAbove, mRight, mBelow) = getAdjacentPipes pm (pipeMapPosToVertex pm startPos)
    adjustStartPipe p = pm & ix y . ix x .~ p

hConnected :: Maybe Pipe -> Maybe Pipe -> Bool
hConnected Nothing _ = False
hConnected _ Nothing = False
hConnected (Just left) (Just right) = left `elem` [WEPipe, NEPipe, SEPipe] && right `elem` [WEPipe, NWPipe, SWPipe]

vConnected :: Maybe Pipe -> Maybe Pipe -> Bool
vConnected Nothing _ = False
vConnected _ Nothing = False
vConnected (Just top) (Just bottom) = top `elem` [NSPipe, SWPipe, SEPipe] && bottom `elem` [NSPipe, NEPipe, NWPipe]

getLoop :: PipeMap -> [Vertex]
getLoop pm = evalState getLoop' (replaceStartPipe pm, [], pipeMapPosToVertex pm . getStartPos $ pm)

getLoop' :: State (PipeMap, [Vertex], Vertex) [Vertex]
getLoop' = do
  (pm, visted, current) <- get
  case getNextNode pm visted current of
    (Just next) -> do
      put (pm, current : visted, next)
      getLoop'
    Nothing -> return (current : visted)

getNextNode :: PipeMap -> [Vertex] -> Vertex -> Maybe Vertex
getNextNode pm visted current
  | hConnected mLeft mCurrent && (pipeMapPosToVertex pm . fromJust $ mLeftPos) `notElem` visted = Just . pipeMapPosToVertex pm . fromJust $ mLeftPos
  | hConnected mCurrent mRight && (pipeMapPosToVertex pm . fromJust $ mRightPos) `notElem` visted = Just . pipeMapPosToVertex pm . fromJust $ mRightPos
  | vConnected mAbove mCurrent && (pipeMapPosToVertex pm . fromJust $ mAbovePos) `notElem` visted = Just . pipeMapPosToVertex pm . fromJust $ mAbovePos
  | vConnected mCurrent mBelow && (pipeMapPosToVertex pm . fromJust $ mBelowPos) `notElem` visted = Just . pipeMapPosToVertex pm . fromJust $ mBelowPos
  | otherwise = Nothing
  where
    mCurrent = Just . getPipe pm . vertexToPipeMapPos pm $ current
    (mLeft, mAbove, mRight, mBelow) = getAdjacentPipes pm current
    (mLeftPos, mAbovePos, mRightPos, mBelowPos) = getAdjacentPipePos pm current

hScanInLoop :: [Vertex] -> PipeMap -> [Vertex]
hScanInLoop loop = concatMap (scanLine [NSPipe, NWPipe, NEPipe] loop) . addVertexToPipeMap

vScanInLoop :: [Vertex] -> PipeMap -> [Vertex]
vScanInLoop loop = concatMap (scanLine [WEPipe, NWPipe, SWPipe] loop) . transpose . addVertexToPipeMap

scanInLoop :: PipeMap -> [Vertex]
scanInLoop pm = hScanInLoop loop pm `intersect` vScanInLoop loop pm
  where
    loop = getLoop pm

addVertexToPipeMap :: PipeMap -> [[(Pipe, Vertex)]]
addVertexToPipeMap pm = zipWith (\pl n -> zip pl [n * length pl ..]) pm [0 ..]

scanLine :: [Pipe] -> [Vertex] -> [(Pipe, Vertex)] -> [Vertex]
scanLine walls loop = fst . foldl (insideFoldFunc walls loop) ([], False)

insideFoldFunc :: [Pipe] -> [Vertex] -> ([Vertex], Bool) -> (Pipe, Vertex) -> ([Vertex], Bool)
insideFoldFunc walls loop (insideVertex, isInside) (currentPipe, vertex)
  | currentPipe `elem` walls && vertex `elem` loop = (insideVertex, not isInside)
  | vertex `notElem` loop && isInside = (vertex : insideVertex, isInside)
  | otherwise = (insideVertex, isInside)
