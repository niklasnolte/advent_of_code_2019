import           Intcode                 hiding ( main )
import qualified Data.List                     as L
import           Input11
import           System.IO.Unsafe               ( unsafePerformIO )

newtype GridRepr = GridRepr [[Int]]

substituteColor :: Int -> String
substituteColor i | i == 0 = " "
                  | i == 1 = "█"

instance Show GridRepr where
  show (GridRepr xx) =
    let x = L.transpose xx
    in  "\n" ++ L.intercalate "\n"
                              (map (L.intercalate "" . map substituteColor) x)

data Grid = Grid {
  _touchedFields :: [Int],
  _repr :: GridRepr,
  _currentPosition :: (Int, Int),
  _currentOrientation :: (Int, Int)
} deriving (Show)

_getIntRepr :: Grid -> [[Int]]
_getIntRepr g = let (GridRepr x) = _repr g in x

rotateDir :: Int -> (Int, Int) -> (Int, Int)
rotateDir input (x, y) = let p = (-1) ^ input in (y * p, -x * p)

testRotateDir :: IO ()
testRotateDir = do
  print $ rotateDir 0 (0, 1)
  print $ rotateDir 0 (1, 0)
  print $ rotateDir 0 (0, -1)
  print $ rotateDir 0 (-1, 0)
  print $ rotateDir 1 (0, 1)
  print $ rotateDir 1 (1, 0)
  print $ rotateDir 1 (0, -1)
  print $ rotateDir 1 (-1, 0)

updatePosition :: Int -> Grid -> Grid
updatePosition input grid = grid
  { _currentPosition    = (xOld + fst newDir, yOld + snd newDir)
  , _currentOrientation = newDir
  }
 where
  (xOld, yOld) = _currentPosition grid
  oldDir       = _currentOrientation grid
  newDir       = rotateDir input oldDir

readCurrentGridColor :: Grid -> Int
readCurrentGridColor grid = _getIntRepr grid !! x !! y
  where (x, y) = _currentPosition grid

paintGrid :: Int -> Grid -> Grid
paintGrid color grid = grid { _repr          = GridRepr newRepr
                            , _touchedFields = newField : _touchedFields grid
                            }
 where
  (x, y)                        = _currentPosition grid
  oldRepr                       = _getIntRepr grid
  (prevRows, target : nextRows) = L.splitAt x oldRepr
  (prev    , _ : next         ) = L.splitAt y target
  updatedTarget                 = prev ++ color : next
  newRepr                       = prevRows ++ updatedTarget : nextRows
  newField                      = x * length target + y

executeRobotCycle :: IO Grid -> IO Computer -> IO (Grid, Computer)
executeRobotCycle ioGrid ioComputer = do
  grid <- ioGrid
  let input = readCurrentGridColor grid
  computer <- ioComputer
  com      <- runProgramWithInput input computer
  let (direction : newColor : _) = _outputs com
  let paintedGrid                = paintGrid newColor grid
  let newGrid                    = updatePosition direction paintedGrid
  return (newGrid, com)

letRobotPaint :: IO Grid -> IO Computer -> IO (Grid, Computer)
letRobotPaint grid computer = do
  (g, c) <- executeRobotCycle grid computer
  case _state c of
    Done -> return (g, c)
    _    -> letRobotPaint (return g) (return c)


initializeGrid :: Int -> Grid
initializeGrid gridWidth = Grid
  { _repr               = GridRepr $ replicate gridWidth $ replicate gridWidth 0
  , _currentPosition    = (gridWidth `div` 2, gridWidth `div` 2)
  , _currentOrientation = (0, -1) {-facing up-}
  , _touchedFields      = []
  }

main = do
  {-testRotateDir-}
  let initializedComputer =
        _default_computer { _sequence = paintingProgram ++ replicate 1000 0 }
  (grid, com) <- letRobotPaint (return $ initializeGrid 1000)
                               (return initializedComputer)
  let answer_1 = length $ L.nub $ _touchedFields grid
  print answer_1
  let correctlyInitializedGrid = paintGrid 1 $ initializeGrid 100
  (grid, _) <- letRobotPaint (return correctlyInitializedGrid)
                             (return initializedComputer)
  print $ _repr grid
