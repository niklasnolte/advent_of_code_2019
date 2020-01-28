import qualified Input18
import Data.Map as M
import Data.Maybe (fromMaybe)
import Data.List as L
import Debug.Trace (trace)

data Direction = North | South | West | East deriving (Eq, Show, Enum, Bounded, Ord)

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


magMH :: Point -> Int
magMH (Point x y) = x+y

minus :: Point -> Point -> Point
minus (Point x1 y1) (Point x2 y2) =
  Point (x1-x2) (y1-y2)

type Key = Char
type Door = Char

data Object = Door Char |
              Key Char |
              Wall |
              EmptySpace |
              Entrance deriving (Eq)

toObject :: Char -> Object
toObject c
  | c == '#' = Wall
  | c == '.' = EmptySpace
  | c == '@' = Entrance
  | c `L.elem` ['a'..'z'] = Key c
  | c `L.elem` ['A'..'Z'] = Door c
  | otherwise = error ("wtf is this? " ++ [c])

instance Show Object where
  show o = case o of
    Wall -> "#"
    EmptySpace -> "."
    Entrance -> "@"
    (Door d) -> [d]
    (Key k) -> [k]

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

data Screen = Screen {
  _getM :: M.Map Point Object
} deriving (Eq)

getElement :: Point -> Screen -> Object
getElement p (Screen m) =
  fromMaybe Wall $ M.lookup p m

toScreen :: [String] -> Screen
toScreen input =
  let buildRow y = [(Point x y, toObject i) |
                    (i, x) <- zip (input!!y) [0..]] in
  Screen $ M.fromList $ L.concat $ L.map buildRow [0..(length input-1)]

getNextPoints :: Point -> [Point]
getNextPoints p = [p `plusDir` d | d <- [(minBound :: Direction)..]]

getNeighbors :: Point -> Screen -> [(Point, Object)]
getNeighbors p s =
  let nps = [p `plusDir` d | d <- [(minBound :: Direction)..]] in
  (\p -> (p, getElement p s)) <$> nps

isDoor :: Object -> Bool
isDoor (Door _) = True
isDoor _ = False

hasNeighborIn :: Point -> [Point] -> Bool
hasNeighborIn p seen =
  let ps = getNextPoints p in
  (> 0) $ sum $ fromEnum <$> (`L.elem` seen) <$> ps

expandPoints :: [Point] -> Screen -> [Point]
expandPoints ps s@(Screen m) =
  let isValid (p,o) = o /= Wall &&
                      not (isDoor o) &&
                      hasNeighborIn p ps &&
                      not (p `L.elem` ps) in
  let newPoints = [x | x <- M.toList m, isValid x] in
  let newPs = (fst <$> newPoints) in
  case length newPs of
  0 -> ps
  _ -> expandPoints (ps ++ newPs) s

getAccessiblePointsFrom :: Point -> Screen -> [Point]
getAccessiblePointsFrom p s@(Screen m) =
  let walkables = expandPoints [p] s in
  let allDoors = [p | (p,o) <- M.toList m, isDoor o] in
  let accessibleDoors = L.filter (`hasNeighborIn` walkables) allDoors in
  walkables ++ accessibleDoors
  

main = do
  let s = toScreen Input18.input
  print s
  print $ getAccessiblePointsFrom (Point 5 1) s
