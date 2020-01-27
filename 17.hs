{-# LANGUAGE QuasiQuotes                      #-}
import qualified Input17
import Intcode hiding (main)
import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe (fromMaybe, fromJust)
import Debug.Trace (trace)
import Data.List.Split (splitOn)
import Data.Char (ord)
import Text.RE.Replace (replaceAll)
import Text.RE.TDFA.String (re,(*=~))

data Direction = North | South | West | East deriving (Eq, Show, Enum, Bounded, Ord)

turnRight :: Direction -> Direction
turnRight d = case d of
  North -> East
  East -> South
  South -> West
  West -> North

turnAround :: Direction -> Direction
turnAround = turnRight . turnRight

turnLeft :: Direction -> Direction
turnLeft = turnAround . turnRight

data Object = Object { _getInt :: Int } deriving (Eq, Ord)

reprObj :: Object -> Char
reprObj = (toEnum :: Int -> Char) . _getInt

instance Show Object where
  show = (:[]) . reprObj

scaffoldElement = Object 35 -- ascii #
emptySpace = Object 46 -- ascii .

data Point = Point {
  _getX :: Int,
  _getY :: Int
} deriving (Show, Eq, Ord)

plusDir :: Point -> Direction -> Point
plusDir (Point x y) d = case d of
  West -> Point (x-1) y
  East -> Point (x+1) y
  North -> Point x (y-1)
  South -> Point x (y+1)

data Screen = Screen {
  _getM :: M.Map Point Object
} deriving (Eq)

getElement :: Point -> Screen -> Object
getElement p (Screen m) =
  fromMaybe emptySpace $ M.lookup p m

minMax :: [Int] -> (Int,Int)
minMax xs = case xs of
  [] -> (0,0)
  _ -> let l:h:[] = sequence [minimum, maximum] xs in (l,h)

getRange_ :: (Point -> Int) -> Screen -> (Int, Int)
getRange_ getAxis (Screen m) =
  let xs = L.map getAxis $ M.keys m in
  minMax xs

getWidth :: Screen -> (Int, Int)
getWidth = getRange_ _getX

getHeight :: Screen -> (Int, Int)
getHeight = getRange_ _getY

instance Show Screen where
  show s =
    let (xLow, xHigh) = getWidth s in
    let (yLow, yHigh) = getHeight s in
    let printRow y = [show $ getElement (Point x y) s
                       | x <- [xLow..xHigh]] in
    let repr = L.map (L.intercalate "" . printRow) [yLow..yHigh] in
    L.intercalate "\n" repr

toScreen :: [Int] -> Screen
toScreen input_ =
  let input = filter (/=[]) $ splitOn [10] {-newline-} input_ in
  let drawLine i = [(Point j i, Object ele) | (j,ele) <- zip [0..] (input!!i) ] in
  let lines = L.map drawLine [0..(length input - 1)] in
  let m = M.fromList $ L.concat lines in
  Screen m

getNeighbors :: Point -> Screen -> [(Direction, Object)]
getNeighbors p s =
  let dirs =  [(minBound::Direction)..] in
  let points = L.map ( p `plusDir`) dirs in
  [(d, getElement p s) | (d,p) <- zip dirs points]

isIntersection :: Screen -> Point -> Bool
isIntersection s p =
  let ns = getNeighbors p s in
  scaffoldElement == getElement p s &&
  (length $ filter ((==scaffoldElement) . snd) ns) == 4

getIntersectionPoints :: Screen -> [Point]
getIntersectionPoints s =
  filter (isIntersection s) $ M.keys $ _getM s

getIntersectionParam :: Point -> Int
getIntersectionParam =
  foldl1 (*) . sequence [_getX, _getY]

part1 :: Computer -> IO Int
part1 c = do
  let nC = run_program c
  let myS = toScreen $ reverse $ _outputs nC
  print myS
  (return . sum . L.map getIntersectionParam . getIntersectionPoints) myS

isDroid :: Object -> Bool
isDroid o = L.elem (reprObj o) "^><v"

getDroidDir :: Object -> Direction
getDroidDir o = case reprObj o of
  '^' -> North
  '>' -> East
  'v' -> South
  '<' -> West
  _ -> error "there is no droid here"

getDroidPos :: Screen -> Point
getDroidPos = fst . fromJust .
              L.find (isDroid . snd) .
              M.toList . _getM

type PathSegment = (Direction, Int)
type Path = [PathSegment]

getSegmentLength :: Point -> Direction -> Screen -> Int
getSegmentLength p d s =
  let points = L.scanl' plusDir p $ repeat d in
  let firstEmpty = fromJust $ L.find ((==emptySpace) . (`getElement` s)) points in
  abs ((_getX p + _getY p) - (_getX firstEmpty + _getY firstEmpty)) - 1


findSegmentToGoNext :: Point -> Direction -> Screen -> Maybe PathSegment
findSegmentToGoNext currPos currDir s =
  let ns = getNeighbors currPos s in
  let movableDirs = L.map fst $ filter ((==scaffoldElement) . snd) ns in
  let movableDirsNotBack = filter (/=(turnAround currDir)) movableDirs in
  case length movableDirsNotBack of
  0 -> Nothing
  1 -> let movD = head movableDirsNotBack in
       Just (movD, getSegmentLength currPos movD s)
  _ -> error "why are there more than 1 movable direction?"

updatePoint :: Point -> Direction -> Int -> Point
updatePoint p d n =
  L.foldl' plusDir p $ L.replicate n d

moveAlongScaffold :: Point -> Direction -> Screen -> Path -> Path
moveAlongScaffold pos dir sc path =
  let nextSegment = findSegmentToGoNext pos dir sc in
  case nextSegment of
  Nothing -> reverse path
  _ -> let ns@(newDir, segLength) = fromJust nextSegment in
       let newPos = uncurry (updatePoint pos) ns in
       moveAlongScaffold newPos newDir sc (fromJust nextSegment:path)

getTurn :: Direction -> Direction -> Char
getTurn d1 d2
  | turnRight d1 == d2 = 'R'
  | turnLeft d1 == d2 = 'L'
  | otherwise = error "Can only turn left and right"

getPathToTraverseScaffold :: Screen -> [String]
getPathToTraverseScaffold s =
  let initDP = getDroidPos s in
  let initDD = getDroidDir $ getElement initDP s in
  let path = moveAlongScaffold initDP initDD s [] in
  let l0 = snd $ head path in
  [((getTurn d1 d2):(show l2)) |
    ((d1,_),(d2,l2)) <- zip ((initDD,l0):init path) path]

getSubLists :: Eq a => [a] -> [[a]]
getSubLists = L.nub . concatMap (tail . L.inits) . L.tails

getNumSubListOccurrences :: Eq a => [a] -> [a] -> Int
getNumSubListOccurrences p l = length $ filter (L.isPrefixOf p) $ L.tails l

placeHolder = "#"

getCompressionScore :: ([String], Int) -> Int
getCompressionScore (seq, nOccs) =
  let maxMem = 20 in
  case L.elem placeHolder seq of
    False ->
      let totalLength = (sum $ L.map ({-count commata-} (+2). length) seq) -1 in
      let neededReprMem = 2*(length seq)-1 in {-count commata too-}
      case totalLength <= maxMem of
        True -> totalLength * nOccs - neededReprMem
        False -> 0
    True -> 0

getSortedFactorizations :: [String] -> [[String]]
getSortedFactorizations path =
  let subLists = getSubLists path in
  let subListsOccs = zip subLists $ L.map (`getNumSubListOccurrences` path) subLists in
  let subListsScores = zip subLists $ L.map getCompressionScore subListsOccs in
  let subListsScoresNoZeros = filter ((> 0) . snd) subListsScores in
  L.map fst $ L.reverse $ L.sortOn snd subListsScoresNoZeros

substitutePlaceholder :: String -> [String] -> [String] -> [String]
substitutePlaceholder ph pattern path =
  let ts = L.tails path in
  let removeN = length pattern in
  let idxToRemove = L.findIndex (L.isPrefixOf pattern) ts in
  case idxToRemove of
  Nothing -> path
  _ -> let idx = fromJust idxToRemove in
       substitutePlaceholder ph pattern $
       (take idx path) ++
       (ph:drop (idx+removeN) path)

getAllNFactorizations :: Int -> [String] -> [[[String]]]
getAllNFactorizations n path =
  let fs = getSortedFactorizations path :: [[String]] in
  let newPaths = [substitutePlaceholder placeHolder f path | f <- fs] :: [[String]] in
  case n of
  1 -> L.transpose [fs]
  _ -> let factorizationsWithPaths = zip fs newPaths in
       let nextFactorizationsFor (f,pWithF) = [f:nextFs |
             nextFs <- getAllNFactorizations (n-1) pWithF] in
       L.concat $ L.map nextFactorizationsFor factorizationsWithPaths

decomposeIntoFunctions :: [String] -> [[String]] -> [String]
decomposeIntoFunctions path factorizations =
  let subst = \p (ph,ptrn) -> substitutePlaceholder [ph] ptrn p in
  let factWithNames = zip ['A'..] factorizations in
  let substitutedPath = L.foldl' subst path factWithNames in
  map (L.intercalate ",") (substitutedPath:factorizations)

formatFun :: String -> String
formatFun fun =
  replaceAll "$1," $ fun *=~ [re|$(["LR"])|]

part2 :: Computer -> Int
part2 c = do
  let nC = run_program c
  let s = toScreen $ reverse $ _outputs nC
  let path = getPathToTraverseScaffold s
  let pRepr = L.intercalate "," path
  let allFact = getAllNFactorizations 3 path
  let possibleCompositions = L.map (decomposeIntoFunctions path) allFact
  let isValidComp = \[m,a,b,c] -> not $ ('L' `L.elem` m) || ('R' `L.elem` m)
  let validCompositions = L.filter isValidComp possibleCompositions
  let oneComp = validCompositions!!0
  let m = (map ord $ head oneComp) ++ [10]
  let [a,b,c] = [ (map ord (formatFun f)) ++ [10] | f <- tail oneComp]
  let nnC = run_program nC { _inputs = m ++ a ++ b ++ c ++ [ord 'n', 10]}
  head $ _outputs nnC

main = do
  let myCom = _default_computer { _sequence = M.fromList $ zip [0..] Input17.input }
  part1 myCom >>= print
  let myComP2 = myCom { _sequence = M.insert 0 2 (_sequence myCom) }
  print $ part2 myComP2
