import System.IO.Unsafe (unsafePerformIO)
import Input10
import qualified Data.List as L
import Data.Ratio ((%))
import Data.Maybe (fromJust)

data Point = Point {
  _getX :: Int,
  _getY :: Int
} deriving (Show, Eq)

type Angle = Float

inBetween :: Ord a => a -> a -> a -> Bool
inBetween x y z = _min <= x && x <= _max where (_min,_max) = (min y z, max y z)

isBetween :: Point -> (Point, Point) -> Bool
isBetween (Point x y) ((Point x1 y1),(Point x2 y2)) =
  (inBetween x x1 x2 && inBetween y y1 y2) &&
  ((x == x1 && x == x2) || ((y%1) == m*(x%1) + b)) {- %1:: Int -> Fractional-}
  where
        m = (y1 - y2) % (x1 - x2)
        b = (y1%1) - m * (x1%1)

transformMapIndexToPoint :: Int -> Map -> Point
transformMapIndexToPoint idx map =
  Point x y
  where w = _width map
        x = idx `mod` w
        y = idx `div` w

transformPointToMapIndex :: Point -> Map -> Int
transformPointToMapIndex (Point x y) map =
  let w = _width map in y*w + x

getAllAsteroids :: Map -> [Point]
getAllAsteroids map =
  [transformMapIndexToPoint idx map | (idx,entry) <- zip [0..] (_repr map), entry == asteroid_sign]

getNumberOfBlockingAsteroids :: Point -> Point -> [Point] -> Int
getNumberOfBlockingAsteroids reference asteroid allAsteroids =
  L.sum $ L.map fromEnum isBlockedBy
  where isBlockedBy = L.map (\x -> isBetween x (reference,asteroid) && x /= reference && x /= asteroid) allAsteroids

countVisibleAsteroids :: Point -> [Point] -> Int
countVisibleAsteroids point allAsteroids =
  length (L.filter isVisible allAsteroids) - 1 {- exclude itself -}
  where  isVisible x = getNumberOfBlockingAsteroids point x allAsteroids == 0

{-part 2-}
getAngleOfConnection :: Point -> Point -> Angle
getAngleOfConnection (Point x1 y1) (Point x2 y2) =
  if angle >= 0 then angle else 2*pi+angle {-a bit twisted to get the definition right-}
  where dx = fromIntegral (x2 - x1) ::Float
        dy = fromIntegral (y1 - y2) ::Float
        angle = atan2 dx dy

test_getAngleOfConnection :: IO()
test_getAngleOfConnection = do
  print $ getAngleOfConnection (Point 1 1) (Point 0 0) * (180 / pi)
  print $ getAngleOfConnection (Point 1 1) (Point 1 0) * (180 / pi)
  print $ getAngleOfConnection (Point 1 1) (Point 2 0) * (180 / pi)
  print $ getAngleOfConnection (Point 1 1) (Point 2 1) * (180 / pi)
  print $ getAngleOfConnection (Point 1 1) (Point 2 2) * (180 / pi)
  print $ getAngleOfConnection (Point 1 1) (Point 1 2) * (180 / pi)
  print $ getAngleOfConnection (Point 1 1) (Point 0 2) * (180 / pi)
  print $ getAngleOfConnection (Point 1 1) (Point 0 1) * (180 / pi)


getDestructionAngleOf :: Point -> Point -> [Point] -> Angle
getDestructionAngleOf target reference allAsteroids =
  (fromIntegral nBlockingOthers * fullAngle) + angle
  where fullAngle = 2 * pi
        angle = getAngleOfConnection reference target
        nBlockingOthers = getNumberOfBlockingAsteroids reference target allAsteroids



sortAsteroidsInOrderOfDestruction :: Point -> [Point] -> [Point]
sortAsteroidsInOrderOfDestruction laserAsteroid otherAsteroids =
  L.sortBy compareByDestructionTime otherAsteroids
  where compareByDestructionTime lhs rhs =
          compare (getDestructionAngleOf lhs laserAsteroid otherAsteroids)
                  (getDestructionAngleOf rhs laserAsteroid otherAsteroids)

main = do
  let allAsteroids = getAllAsteroids real_input
  let nVisibleAsteroids = map (`countVisibleAsteroids` allAsteroids) allAsteroids
  let answer1 = maximum nVisibleAsteroids
  print answer1
  let laserPositionIndex = fromJust $ L.elemIndex answer1 nVisibleAsteroids
  let laserAsteroid = allAsteroids !! laserPositionIndex
  let allOtherAsteroids = take laserPositionIndex allAsteroids ++ drop (laserPositionIndex + 1) allAsteroids
  let answer2 = sortAsteroidsInOrderOfDestruction laserAsteroid allOtherAsteroids !! 199
  print answer2

