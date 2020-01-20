import Intcode hiding (main)
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.List.Split as S
import Control.Monad
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

data Point = Point Int Int deriving (Show, Eq, Ord)

newtype Screen = Screen (M.Map Point TileType)

type Instructions = [(Point, TileType)]

intercalateEveryN :: Int -> [a] -> [a] -> [a]
intercalateEveryN n entry list = L.intercalate entry $ S.chunksOf n list

getWidth :: Screen -> Int
getWidth (Screen s) = maximum [x | (Point x y) <- map fst $ M.toList s]

instance Show Screen where
  show (Screen s) = intercalateEveryN (getWidth $ Screen s) "\n" $
                    L.intercalate "" $
                    L.map (show . snd) $
                    M.toList s

buildScreen :: [Int] -> IO Screen
buildScreen input = do
  let com = _default_computer { _sequence = input ++ replicate 1000 0 }
  resultCom <- run_program com
  let outputs = S.chunksOf 3 $ _outputs resultCom
  print outputs
  let instr = [ (Point x y, toEnum t::TileType)  | [t,y,x] <- outputs]
  return $ Screen $ M.fromList instr

countBlockTiles :: Screen -> Int
countBlockTiles (Screen s) =
  sum $ map (fromEnum . (==Block) . snd) $ M.toList s

main = do
  screen <- buildScreen input
  print screen
  print $ countBlockTiles screen
  {-print $ Screen $ M.fromList [((Point 0 0), Empty), ((Point 0 1), Wall), ((Point 1 0), Block)]-}
  {-print $ intercalateEveryN 2 "," "abcdefgh"-}

