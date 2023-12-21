{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module AdventOfCode.Day20 (day20, part1, part2, runNetwork2) where

import Control.Monad.State (MonadState (get), State, evalState, gets, modify, runState, when)
import Data.Either (fromRight)
import Data.Map (Map, empty, insert, (!?))
import qualified Data.Map as Map (fromList)
import Data.Sequence (Seq ((:<|), (:|>)), replicateM, singleton, (><))
import qualified Data.Sequence as Seq (empty, fromList)
import Data.Void (Void)
import Lens.Micro ((&), (+~), (.~), _1, _2)
import Text.Megaparsec (MonadParsec (eof, try), Parsec, parse, sepBy, some, (<|>))
import Text.Megaparsec.Char (char, eol, letterChar, string)

day20 :: IO ()
day20 = do
  input <- readFile "src/Data/Day20.txt"
  putStr "Day 20 part 1: "
  print . part1 $ input
  putStr "Day 20 part 2: "
  print . part2 $ input

part1 :: String -> Int
part1 input = low * high
  where
    modules = fromRight (error "Can't parse input") (parse inputParser "" input)
    (_ :|> (low, high)) = evalState (replicateM 1000 (pushButton >> runNetwork)) . initNetworkState . initNetwork $ modules

part2 :: String -> Int
part2 _ = undefined -- evalState runNetwork2 . initNetworkState . initNetwork $ modules
-- where
--  modules = fromRight (error "Can't parse input") (parse inputParser "" input)

type Parser = Parsec Void String

data Intensity = Low | High deriving (Show, Eq)

data Signal where
  Signal :: {_intensity :: Intensity, _source :: String, _destination :: String} -> Signal
  deriving (Show, Eq)

data Module where
  FlipFlop :: {_name :: String, _outputs :: [String], _On :: Bool} -> Module
  Conjunction :: {_name :: String, _outputs :: [String], _memory :: Map String Intensity} -> Module
  Broadcast :: {_name :: String, _outputs :: [String]} -> Module
  deriving (Show, Eq)

type Network = Map String Module

data NetworkState where
  NetworkState :: {_network :: Network, _signalQueue :: Seq Signal, _intensityCount :: (Int, Int), _buttonPresses :: Int} -> NetworkState
  deriving (Show, Eq)

outputParser :: Parser [String]
outputParser = string " -> " *> some letterChar `sepBy` string ", "

broadcasterParser :: Parser Module
broadcasterParser = Broadcast <$> string "broadcaster" <*> outputParser <* eol

flipFlopParser :: Parser Module
flipFlopParser = FlipFlop <$> (char '%' *> some letterChar) <*> outputParser <*> pure False <* eol

conjunctionParser :: Parser Module
conjunctionParser = Conjunction <$> (char '&' *> some letterChar) <*> outputParser <*> pure empty <* eol

moduleParser :: Parser Module
moduleParser = try broadcasterParser <|> try flipFlopParser <|> try conjunctionParser

inputParser :: Parser [Module]
inputParser = some moduleParser <* eol <* eof

initNetwork :: [Module] -> Network
initNetwork modules = Map.fromList . zip (map _name modules) . map (initConjunctionInput modules) $ modules

initConjunctionInput :: [Module] -> Module -> Module
initConjunctionInput modules conjunction@(Conjunction name _ _) = conjunction {_memory = Map.fromList (map (,Low) inputs)}
  where
    inputs = map _name . filter (\m -> name `elem` _outputs m) $ modules
initConjunctionInput _ m = m

pushButton :: State NetworkState ()
pushButton = do
  modify incrementButtonPress'
  pushSignals . singleton $ Signal {_source = "Button", _destination = "broadcaster", _intensity = Low}
  where
    incrementButtonPress' s = s {_buttonPresses = succ . _buttonPresses $ s}

runNetwork :: State NetworkState (Int, Int)
runNetwork = do
  queue <- gets _signalQueue
  if null queue
    then gets _intensityCount
    else do
      signal <- popSignal
      if _intensity signal == Low
        then incrementLowIntensity
        else incrementHighIntensity
      network <- gets _network
      let maybeModule = network !? _destination signal
      case maybeModule of
        Nothing -> runNetwork -- The signal ends in an undefined module
        (Just m) -> do
          let (signals, m') = handleSignal signal m
          updateModule m'
          pushSignals . Seq.fromList $ signals
          runNetwork

runNetwork2 :: String -> State NetworkState Int
runNetwork2 moduleName = do
  queue <- gets _signalQueue
  if null queue
    then do
      (_, high) <- gets _intensityCount
      if high == 1
        then gets _buttonPresses
        else do
          resetHighIntensity
          resetLowIntensity
          pushButton
          runNetwork2 moduleName
    else do
      signal <- popSignal
      when (_source signal == moduleName) $
        if _intensity signal == Low
          then incrementLowIntensity
          else incrementHighIntensity
      network <- gets _network
      let maybeModule = network !? _destination signal
      case maybeModule of
        Nothing -> runNetwork2 moduleName -- The signal ends in an undefined module
        (Just m) -> do
          let (signals, m') = handleSignal signal m
          updateModule m'
          pushSignals . Seq.fromList $ signals
          runNetwork2 moduleName

incrementHighIntensity :: State NetworkState ()
incrementHighIntensity = modify incrementHighIntensity'
  where
    incrementHighIntensity' s = s {_intensityCount = _intensityCount s & _2 +~ 1}

resetHighIntensity :: State NetworkState ()
resetHighIntensity = modify resetHighIntensity'
  where
    resetHighIntensity' s = s {_intensityCount = _intensityCount s & _2 .~ 0}

incrementLowIntensity :: State NetworkState ()
incrementLowIntensity = modify incrementLowIntensity'
  where
    incrementLowIntensity' s = s {_intensityCount = _intensityCount s & _1 +~ 1}

resetLowIntensity :: State NetworkState ()
resetLowIntensity = modify resetLowIntensity'
  where
    resetLowIntensity' s = s {_intensityCount = _intensityCount s & _1 .~ 0}

updateModule :: Module -> State NetworkState ()
updateModule m = modify updateModule'
  where
    updateModule' networkState = networkState {_network = insert (_name m) m (_network networkState)}

pushSignals :: Seq Signal -> State NetworkState ()
pushSignals signals = modify pushSignals'
  where
    pushSignals' s = s {_signalQueue = _signalQueue s >< signals}

popSignal :: State NetworkState Signal
popSignal = do
  queue <- gets _signalQueue
  let (signal :<| queue') = queue
  modify (\s -> s {_signalQueue = queue'})
  return signal

handleSignal :: Signal -> Module -> ([Signal], Module)
handleSignal signal broadcast = case broadcast of
  (Broadcast name outputs) -> (map (Signal Low name) outputs, broadcast)
  flipFlop@(FlipFlop {}) -> runState (handleFlipflop signal) flipFlop
  conjunction@(Conjunction {}) -> runState (handleConjunction signal) conjunction

handleFlipflop :: Signal -> State Module [Signal]
handleFlipflop signal =
  if _intensity signal == Low
    then do
      m <- get
      let intensity = if _On m then Low else High
      modify (\m' -> m' {_On = not . _On $ m'})
      return . map (Signal intensity (_name m)) . _outputs $ m
    else return []

handleConjunction :: Signal -> State Module [Signal]
handleConjunction signal = do
  name <- gets _name
  outputs <- gets _outputs
  memory <- gets _memory
  let memory' = insert (_source signal) (_intensity signal) memory
  modify (\m -> m {_memory = memory'})
  let intensity = if all (== High) memory' then Low else High
  return . map (Signal intensity name) $ outputs

initNetworkState :: Network -> NetworkState
initNetworkState network =
  NetworkState
    { _network = network,
      _signalQueue = Seq.empty,
      _intensityCount = (0, 0),
      _buttonPresses = 0
    }
