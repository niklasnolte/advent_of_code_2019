import qualified Data.Vector as V
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Debug.Trace (trace)
import Control.Applicative ((<*>))
import Control.Monad (sequence)

data Moon = Moon {
  _name :: String,
  _getPos :: V.Vector3,
  _getVelo :: V.Vector3
} deriving (Eq, Ord, Show)

{-instance Show Moon where-}
{-  show (Moon name pos velo) = name ++ " " ++ (show velo)-}

frozenMoon = Moon {
  _name = "DEFAULT",
  _getVelo = V.Vector3 0 0 0,
  _getPos = V.Vector3 0 0 0
}

initializeMoons :: [Moon]
initializeMoons =
  m0:m1:m2:m3:[]
  where m0 = frozenMoon { _name = "Io",
                          _getPos = V.Vector3 (-9) 10 (-1) }
        m1 = frozenMoon { _name = "Ganymed",
                          _getPos = V.Vector3 (-14) (-8) 14 }
        m2 = frozenMoon { _name = "Calypso",
                          _getPos = V.Vector3 1 5 6 }
        m3 = frozenMoon { _name = "Europa",
                          _getPos = V.Vector3 (-19) 7 8 }

updateVelo :: (Moon,Moon) -> (V.Vector3, V.Vector3)
updateVelo (m1,m2) = (-upd, upd)
  where posDiff = (_getPos m1) - (_getPos m2)
        upd = signum posDiff

updateVelocity_ :: [(Moon, Moon)] -> [(Moon, V.Vector3)] -> [(Moon, V.Vector3)]
updateVelocity_ [] updates = updates
updateVelocity_ pairs updates =
  updateVelocity_ rest ((m1, m1Upd):(m2, m2Upd):updates)
  where ((m1,m2):rest) = pairs
        (m1Upd, m2Upd) = updateVelo (m1,m2)

updateVelocity :: [Moon] -> [Moon]
updateVelocity moons =
  [m { _getVelo = (_getVelo m) + (fromJust $ m `M.lookup` updates) } | m <- moons]
  where pairs = L.nub [(x,y) | x <- moons, y <- moons, x < y]
        updates = M.fromListWith (+) $ updateVelocity_ pairs []

updatePosition :: [Moon] -> [Moon]
updatePosition moons = [m { _getPos = (_getPos m) + (_getVelo m) } | m <- moons]


calculateEnergy :: (Moon -> V.Vector3) -> Moon -> Double
calculateEnergy fun moon =
  (V.v3x v) + (V.v3y v) + (V.v3z v)
  where v = abs (fun moon)

calculatePotentialEnergy :: Moon -> Double
calculatePotentialEnergy = calculateEnergy _getPos

calculateKineticEnergy :: Moon -> Double
calculateKineticEnergy = calculateEnergy _getVelo

calculateTotalEnergy :: Moon -> Double
calculateTotalEnergy = (foldl1 (*)) . sequence [calculateKineticEnergy, calculatePotentialEnergy]

stepOnce :: [Moon] -> [Moon]
stepOnce = updatePosition . updateVelocity

stepNTimes :: Int -> [Moon] -> [Moon]
stepNTimes 0 moons = moons
stepNTimes i moons = stepNTimes (i-1) $ stepOnce moons

data Dimension = X|Y|Z deriving (Show,Enum,Eq)
type VisitedStates = S.Set [(Double,Double)]

getStates :: Dimension -> Moon -> (Double,Double)
getStates dim moon = case dim of
  X ->  (V.v3x $ _getPos moon, V.v3x $ _getVelo moon)
  Y ->  (V.v3y $ _getPos moon, V.v3y $ _getVelo moon)
  Z ->  (V.v3z $ _getPos moon, V.v3z $ _getVelo moon)

recordStates :: Dimension -> [Moon] -> VisitedStates -> VisitedStates
recordStates dim moons states =
  let newEntry = L.map (getStates dim) moons in
  S.insert newEntry states


stepOnceAndRecordState :: Dimension -> [Moon] -> VisitedStates -> ([Moon], VisitedStates)
stepOnceAndRecordState dim moons states =
  let newMoons = stepOnce moons in
  let newStates = recordStates dim newMoons states in
  (newMoons, newStates)

stepWhileUniqueStates :: Int -> Dimension -> [Moon] -> VisitedStates -> Int
stepWhileUniqueStates i dim moons states
  | length states == i = let (newMoons, newStates) = stepOnceAndRecordState dim moons states in
                         stepWhileUniqueStates (i+1) dim newMoons newStates
  | otherwise = length states


main :: IO ()
main = do
  let moons = stepNTimes 1000 initializeMoons
  let answer_1 = sum $ L.map calculateTotalEnergy moons
  print answer_1
  let px:py:pz:[] = map (\d -> stepWhileUniqueStates 0 d initializeMoons S.empty) [X,Y,Z] 
  let answer_2 = lcm (lcm px py) pz
  print answer_2

