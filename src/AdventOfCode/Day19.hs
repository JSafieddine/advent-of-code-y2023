{-# LANGUAGE GADTs #-}

module AdventOfCode.Day19 (day19, part1, part2) where

import Control.Monad.State (State, evalState, get, gets, modify, put)
import Data.Functor (($>))
import Data.Map (Map, adjust, foldr, fromList, (!))
import Data.Maybe (fromJust, isJust, mapMaybe)
import Data.Void (Void)
import Text.Megaparsec (Parsec, between, eof, parse, sepBy, some, try, (<|>))
import Text.Megaparsec.Char (char, eol, letterChar)
import Text.Megaparsec.Char.Lexer (decimal)
import Prelude hiding (foldr)

day19 :: IO ()
day19 = do
  input <- readFile "src/Data/Day19.txt"
  putStr "Day 19 part 1: "
  print . part1 $ input
  putStr "Day 19 part 2: "
  print . part2 $ input

part1 :: String -> Int
part1 input = sum . map partRating . getAcceptedParts wfs $ parts
  where
    (wfs, parts) = case parse inputParser "" input of
      (Right result) -> result
      (Left _) -> error "Can't parse input!"

part2 :: String -> Integer
part2 input = sum . map universalPartRating . evalState workUniversalPart . initUniversalWorkflowState $ wfs
  where
    (wfs, _) = case parse inputParser "" input of
      (Right result) -> result
      (Left _) -> error "Can't parse input!"

data Part where
  Part :: {_rating :: Map Category Int} -> Part
  deriving (Show)

data Category = X | M | A | S deriving (Show, Eq, Ord)

data Workflow where
  Workflow :: {_name :: String, _rules :: [Rule]} -> Workflow
  deriving (Show)

data WorkflowState where
  WorkflowState :: {_workflows :: Map String Workflow, _currentWorkflow :: String, _part :: Part} -> WorkflowState

type Parser = Parsec Void String

categoryParser :: Parser Category
categoryParser = char 'x' $> X <|> char 'm' $> M <|> char 'a' $> A <|> char 's' $> S

ltRuleParser :: Parser Rule
ltRuleParser = LTRule <$> (categoryParser <* char '<') <*> (decimal <* char ':') <*> some letterChar

gtRuleParser :: Parser Rule
gtRuleParser = GTRule <$> (categoryParser <* char '>') <*> (decimal <* char ':') <*> some letterChar

catchAllRuleParser :: Parser Rule
catchAllRuleParser = CatchAllRule <$> some letterChar

ruleParser :: Parser Rule
ruleParser = try ltRuleParser <|> try gtRuleParser <|> catchAllRuleParser

applyRule :: Rule -> Part -> Maybe String
applyRule (CatchAllRule nextWorkFlow) _ = Just nextWorkFlow
applyRule (LTRule cat threshhold nextWorkFlow) part = if _rating part ! cat < threshhold then Just nextWorkFlow else Nothing
applyRule (GTRule cat threshhold nextWorkFlow) part = if _rating part ! cat > threshhold then Just nextWorkFlow else Nothing

combineRules :: [Part -> Maybe String] -> (Part -> String)
combineRules rules part = head . mapMaybe ($ part) $ rules

workFlowParser :: Parser Workflow
workFlowParser = Workflow <$> some letterChar <*> between (char '{') (char '}') (ruleParser `sepBy` char ',') <* eol

categoryAmountParser :: Parser (Category, Int)
categoryAmountParser = (,) <$> categoryParser <* char '=' <*> decimal

partParser :: Parser Part
partParser = Part . fromList <$> between (char '{') (char '}') (categoryAmountParser `sepBy` char ',') <* eol

inputParser :: Parser ([Workflow], [Part])
inputParser = (,) <$> (some workFlowParser <* eol) <*> some partParser <* eol <* eof

workPart :: State WorkflowState Bool
workPart = do
  wfName <- gets _currentWorkflow
  case wfName of
    "A" -> return True
    "R" -> return False
    _ -> do
      wfs <- gets _workflows
      part <- gets _part
      let workflow = wfs ! wfName
      let nextWorkflow = (combineRules . map applyRule . _rules $ workflow) part
      modify (\st -> st {_currentWorkflow = nextWorkflow})
      workPart

initWorkflowState :: [Workflow] -> Part -> WorkflowState
initWorkflowState wfs part = WorkflowState {_workflows = wfMap, _currentWorkflow = "in", _part = part}
  where
    wfMap = fromList . zip (map _name wfs) $ wfs

getAcceptedParts :: [Workflow] -> [Part] -> [Part]
getAcceptedParts wfs = filter (evalState workPart . initState)
  where
    initState = initWorkflowState wfs

partRating :: Part -> Int
partRating = sum . _rating

data Rule where
  LTRule :: {_category :: Category, _threshhold :: Int, _workflow :: String} -> Rule
  GTRule :: {_category :: Category, _threshhold :: Int, _workflow :: String} -> Rule
  CatchAllRule :: {_workflow :: String} -> Rule
  deriving (Show)

type Interval = (Int, Int)

type UniversalPart = Map Category Interval

isIn :: Int -> Interval -> Bool
isIn x (start, end) = start <= x && x <= end

adjustUPartIntervalStart :: UniversalPart -> Category -> Int -> UniversalPart
adjustUPartIntervalStart upart cat th = adjust adjustIntervalStart cat upart
  where
    adjustIntervalStart (_, end) = (th, end)

adjustUPartIntervalEnd :: UniversalPart -> Category -> Int -> UniversalPart
adjustUPartIntervalEnd upart cat th = adjust adjustIntervalEnd cat upart
  where
    adjustIntervalEnd (start, _) = (start, th)

splitUniversalPart :: UniversalPart -> Rule -> (Maybe (UniversalPart, String), Maybe UniversalPart)
splitUniversalPart upart (CatchAllRule wf) = (Just (upart, wf), Nothing)
splitUniversalPart upart (LTRule cat th wf) =
  if th `isIn` (upart ! cat)
    then (Just (adjustUPartIntervalEnd upart cat (th - 1), wf), Just . adjustUPartIntervalStart upart cat $ th)
    else (Nothing, Just upart)
splitUniversalPart upart (GTRule cat th wf) =
  if th `isIn` (upart ! cat)
    then (Just (adjustUPartIntervalStart upart cat (th + 1), wf), Just . adjustUPartIntervalEnd upart cat $ th)
    else (Nothing, Just upart)

splitUniversalParts :: UniversalPart -> [Rule] -> [(UniversalPart, String)]
splitUniversalParts _ [] = error "Not all parts were mapped!"
splitUniversalParts upart (rule : rules)
  | isJust maybeMapped && isJust maybeRest = fromJust maybeMapped : splitUniversalParts (fromJust maybeRest) rules
  | isJust maybeMapped = return . fromJust $ maybeMapped
  | otherwise = splitUniversalParts (fromJust maybeRest) rules
  where
    (maybeMapped, maybeRest) = splitUniversalPart upart rule

data UniversalWorkflowState where
  UniversalWorkflowState :: {_uWorkflows :: Map String Workflow, _uParts :: [(UniversalPart, String)], _accepted :: [UniversalPart]} -> UniversalWorkflowState
  deriving (Show)

workUniversalPart :: State UniversalWorkflowState [UniversalPart]
workUniversalPart = do
  uparts <- gets _uParts
  if null uparts
    then gets _accepted
    else do
      (upart, wfName) <- popUPart
      wfs <- gets _uWorkflows
      let workflow = wfs ! wfName
      let (newUparts, accepted) = sortUparts . splitUniversalParts upart $ _rules workflow
      pushUParts newUparts
      pushAccepted accepted
      workUniversalPart

popUPart :: State UniversalWorkflowState (UniversalPart, String)
popUPart = do
  uPartState <- get
  let upart = head . _uParts $ uPartState
  put uPartState {_uParts = tail . _uParts $ uPartState}
  return upart

pushUParts :: [(UniversalPart, String)] -> State UniversalWorkflowState ()
pushUParts uparts = modify (\st -> st {_uParts = _uParts st ++ uparts})

pushAccepted :: [UniversalPart] -> State UniversalWorkflowState ()
pushAccepted uparts = modify (\st -> st {_accepted = _accepted st ++ uparts})

sortUparts :: [(UniversalPart, String)] -> ([(UniversalPart, String)], [UniversalPart])
sortUparts [] = ([], [])
sortUparts ((upart, "A") : rest) = ([], [upart]) <> sortUparts rest
sortUparts ((_, "R") : rest) = sortUparts rest
sortUparts (upart : rest) = ([upart], []) <> sortUparts rest

intervalLength :: Interval -> Integer
intervalLength (start, end) = toInteger end - toInteger start + 1

universalPartRating :: UniversalPart -> Integer
universalPartRating = foldr ((*) . intervalLength) 1

initUniversalWorkflowState :: [Workflow] -> UniversalWorkflowState
initUniversalWorkflowState wfs = UniversalWorkflowState {_uWorkflows = wfMap, _uParts = [(fromList [(X, (1, 4000)), (M, (1, 4000)), (A, (1, 4000)), (S, (1, 4000))], "in")], _accepted = []}
  where
    wfMap = fromList . zip (map _name wfs) $ wfs
