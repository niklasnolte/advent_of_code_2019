import qualified Input6
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import qualified Data.Map as Map
import Control.Applicative as Ap
import Data.List (findIndex, zipWith)

type SpaceObject = String
type Orbit = (SpaceObject, SpaceObject)
type OrbitMap = Map.Map SpaceObject [SpaceObject]

_BASE = "COM" :: SpaceObject

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

traverse_and_count_orbits :: OrbitMap -> Int -> SpaceObject -> Int
traverse_and_count_orbits my_map current_depth current_fixed
  = let movings = Map.lookup current_fixed my_map in
    let traversal = traverse_and_count_orbits my_map (current_depth+1) in
    case movings of Nothing -> current_depth
                    otherwise ->  let v = map traversal $ fromJust movings in
                                  (sum v) + current_depth


orbits_around :: SpaceObject -> [Orbit] -> Maybe SpaceObject
orbits_around current_object all_orbits =
  case base of [] -> Nothing
               (sun, planet):[] -> Just sun
  where base = filter (\(_, planet) -> current_object == planet) all_orbits


trace_to_base :: SpaceObject -> [Orbit] -> [Orbit] -> [Orbit]
trace_to_base current_planet my_orbits all_orbits =
  case sun of Nothing -> my_orbits
              otherwise -> trace_to_base (fromJust sun) next all_orbits
  where sun = orbits_around current_planet all_orbits :: Maybe SpaceObject
        next = (fromJust sun, current_planet):my_orbits

find_closest_common_base :: [Orbit] -> [Orbit] -> Maybe Int
find_closest_common_base trace1 trace2 =
  findIndex (==False) $ zipWith (==) trace1 trace2


main = do let all_orbits =  _parse_orbits Input6.input
          let orbit_map = get_orbit_map all_orbits
          let answer_1 = traverse_and_count_orbits orbit_map 0 _BASE
          print answer_1
          let me_to_base = trace_to_base "YOU" [] all_orbits
          let santa_to_base = trace_to_base "SAN" [] all_orbits
          let first_diff = fromJust $ find_closest_common_base me_to_base santa_to_base
          let answer_2 = (length me_to_base) + (length santa_to_base) - 2*first_diff - 2
          print answer_2
          
