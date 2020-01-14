import Intcode hiding (main)
import qualified Data.List as L
import Input11

data GridRepr = GridRepr [[Int]]

substitute_color :: Int -> String
substitute_color i
  | i == 0 = " "
  | i == 1 = "█"

instance Show GridRepr where
  show (GridRepr x) = "\n" ++ (L.intercalate "\n" $ map (\y -> L.intercalate "" $ map substitute_color y) x)

data Grid = Grid {
  _repr :: GridRepr,
  _currentPosition :: (Int, Int),
  _currentOrientation :: (Int, Int)
} deriving (Show)

_getIntRepr :: Grid -> [[Int]]
_getIntRepr g = let (GridRepr x) = _repr g in x

rotateDir :: Int -> (Int, Int) -> (Int, Int)
rotateDir input (x,y) = let p = (-1)^input in (y*p, -x*p)

updatePosition ::  Int -> Grid -> Grid
updatePosition input grid =
  grid { _currentPosition = (xOld+xOldDir, yOld+yOldDir),
         _currentOrientation = newDir }
  where (xOld,yOld) = _currentPosition grid
        (xOldDir, yOldDir) = _currentOrientation grid
        newDir = rotateDir input (xOldDir, yOldDir)

readCurrentGridColor :: Grid -> Int
readCurrentGridColor grid =
  _getIntRepr grid !! x !! y
  where (x,y) = _currentPosition grid

paintGrid :: Int -> Grid -> Grid
paintGrid color grid =
  grid { _repr = GridRepr newRepr }
  where (x,y) = _currentPosition grid
        oldRepr = _getIntRepr grid
        (prevRows,target:nextRows) = L.splitAt x oldRepr
        (prev,_:next) = L.splitAt y target
        updatedTarget = prev ++ color:next
        newRepr = prevRows ++ updatedTarget:nextRows

executeRobotCycle :: IO Grid -> IO Computer -> IO (Grid, Computer)
executeRobotCycle ioGrid ioComputer = do
  grid <- ioGrid
  let input = readCurrentGridColor grid
  computer <- ioComputer
  com <- runProgramWithInput input computer
  let (newColor:direction:_) = _outputs com
  let paintedGrid = paintGrid newColor grid
  let newGrid = updatePosition direction paintedGrid
  return (newGrid, com)

letRobotPaint :: Int -> IO Grid -> IO Computer -> IO (Grid, Computer)
letRobotPaint i grid computer = do
  (g,c) <- executeRobotCycle grid computer
  print $ (_sequence c) !! (_current_position c)
  {-print g-}
  {-print c-}
  case _done c of
    True -> return (g,c)
    False -> if (i < -1) then return (g,c) else letRobotPaint (i+1) (return g) (return c)

initializedGrid = Grid { _repr = GridRepr $ take 50 $ repeat $ take 50 $ repeat 0,
                         _currentPosition = (25, 25),
                         _currentOrientation = (0,-1) }{-facing up-} 

main = do
  {-print initializedGrid-}
  let initializedComputer = _default_computer { _sequence = paintingProgram }
  (grid, com) <- letRobotPaint 0 (return initializedGrid) (return initializedComputer)
  print grid
  {-print com-}
