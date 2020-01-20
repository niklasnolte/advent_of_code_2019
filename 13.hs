import Intcode hiding (main)
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Maybe as Maybe
import Input13
import Debug.Trace (trace)

data TileType = Empty | Wall | Block | HPaddle | Ball deriving (Enum, Eq)

instance Show TileType where
  show t = case t of
    Empty -> " "
    Wall -> "█"
    Block -> "X"
    HPaddle -> "―"
    Ball -> "O"

data Point = Point Int Int deriving (Show, Eq)

instance Ord Point where
  compare (Point x1 y1) (Point x2 y2) = compare (y1, x1) (y2, x2)

data Screen = Screen {
  getMap :: M.Map Point TileType,
  getScore :: Int
}

defaultScreen = Screen { getMap = M.empty, getScore = 0 }

type Instructions = [(Point, TileType)]


chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n x = take n x : chunksOf n (drop n x)

intercalateEveryN :: Int -> [a] -> [a] -> [a]
intercalateEveryN n entry list = L.intercalate entry $ chunksOf n list

getWidth :: Screen -> Int
getWidth (Screen m _) = let y = [x | (Point x y) <- map fst $ M.toList m] in maximum y + 1

instance Show Screen where
  show sc@(Screen m s) = "Score: " ++ show s ++ "\n" ++
                         intercalateEveryN (getWidth sc) "\n" (
                         L.intercalate "" $
                         L.map (show . snd) $
                         M.toList m)

updateScreen :: [Int] -> Screen -> Screen
updateScreen newComOutput (Screen m s) =
  let chunks = chunksOf 3 newComOutput in
  let instr = [(Point x y, toEnum t::TileType)  | [t,y,x] <- chunks, x /= -1] in
  let score = L.find (\[t,y,x] -> x == (-1) && y == 0) chunks in
  Screen { getMap = M.fromList instr `M.union` m, getScore = Maybe.maybe s head score}

gameStep :: Int -> Computer -> Screen -> (Computer, Screen)
gameStep input com screen =
  let newCom = run_program com { _inputs = [input], _outputs = [] } in
  let newOutputs = _outputs newCom in
  let s = updateScreen newOutputs screen in
  (newCom, s)


getTileIndexFromOutputs :: [Int] -> TileType -> Maybe Int
getTileIndexFromOutputs outputs t =
  let outputsWithIndex = zip outputs [0..] in
  L.findIndex (\(x, idx) -> x == fromEnum t && idx `mod` 3 == 0) outputsWithIndex

determineMovement :: Computer -> Int
determineMovement com =
  signum( xBall - xHpaddle)
  where outputs = _outputs com
        ballIndex = getTileIndexFromOutputs outputs Ball
        hpaddleIndex =  getTileIndexFromOutputs outputs HPaddle
        xBall = Maybe.maybe 0 (\x -> outputs !!(x+2)) ballIndex
        xHpaddle = Maybe.maybe 0 (\x -> outputs !!(x+2)) hpaddleIndex

countBlockTiles :: Screen -> Int
countBlockTiles (Screen m _) =
  sum $ map (fromEnum . (==Block) . snd) $ M.toList m

runGame :: Int -> Computer -> Screen -> Screen
runGame i com screen =
  let input = determineMovement com in
  let (c,s) = gameStep input com screen in
  case _state c of
    Done -> s
    _ -> case countBlockTiles s of
           0 -> s
           _ -> runGame (i+1) c s


main = do
  let com = _default_computer { _sequence = input ++ replicate 100 0 }
  let resultCom = run_program com
  let screen = updateScreen (_outputs resultCom) defaultScreen
  print $ countBlockTiles screen
  {- part 2-}
  let newInput = 2 : drop 1 input
  let com = _default_computer { _sequence = newInput ++ replicate 100 0 }
  print $ runGame 0 com defaultScreen
