import Input6
import Control.Monad (sequence)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import qualified Data.Map as Map

type Orbit = (String, String)
type OrbitMap = Map.Map String [String]


_parse_orbit :: String -> Orbit
_parse_orbit = (\[x,y] -> (x,y)) . splitOn ")"

_parse_orbits :: [String] -> [Orbit]
_parse_orbits = fmap _parse_orbit

_insert_into_orbit_map :: Orbit -> OrbitMap -> OrbitMap
_insert_into_orbit_map (key, new) current_map = Map.insertWith (++) key [new] current_map

_orbit_map :: [Orbit] -> OrbitMap -> OrbitMap
_orbit_map (next:orbits) map = _orbit_map orbits $ _insert_into_orbit_map next map
_orbit_map [] map = map

get_orbit_map :: [Orbit] -> OrbitMap
get_orbit_map orbits = _orbit_map orbits $ Map.fromList []

traverse_and_count_orbits :: OrbitMap -> Int -> String -> Int
traverse_and_count_orbits my_map current_depth current_fixed
  = let movings = Map.lookup current_fixed my_map in
    let traversal = traverse_and_count_orbits my_map (current_depth+1) in
    case movings of Nothing -> current_depth
                    otherwise ->  let v = map traversal $ fromJust movings in
                                  (sum v) + current_depth


main = do let orbit_map = get_orbit_map $ _parse_orbits input
          let answer_1 = traverse_and_count_orbits orbit_map 0 "COM"
          print answer_1
