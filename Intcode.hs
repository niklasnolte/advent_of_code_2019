module Intcode where
import qualified Data.Map as M
import qualified Data.Maybe as Maybe
import Debug.Trace (trace)

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

data State = Running | WaitingForInput | Done deriving (Enum, Show, Eq)

data Computer =
  Computer {
    _inputs           :: [Int],
    _outputs          :: [Int],
    _current_position :: Int,
    _sequence         :: M.Map Int Int,
    _state            :: State,
    _relative_base    :: Int
  } deriving (Show)

_default_computer = Computer { _inputs = [],
                               _outputs = [],
                               _current_position = 0,
                               _sequence = M.empty,
                               _state = Running,
                               _relative_base = 0 }

data Mode = Position | Immediate | Relative deriving (Enum, Show, Eq)

getVal x seq = Maybe.fromJust $ M.lookup x seq

parse_opcode opcode = (op, modes)
                      where {
                        op = opcode `mod` 100;
                        modes_as_int =  map read $ reverse $ map (:[]) $ show $ opcode `div` 100 ::[Int];
                        modes = map (\x -> toEnum x :: Mode) modes_as_int;
                      }


get_values_by_mode com modes n =
  [ get_val v mode | (v,mode) <- zip (map (\i -> getVal i seq) [start..(n+start-1)]) (modes ++ repeat Position) ]
  where start = _current_position com + 1
        seq = _sequence com
        get_val v mode = case mode of
          Position -> getVal v seq
          Immediate -> v
          Relative -> getVal (v + _relative_base com) seq

get_position_by_mode seq pos rel mode = case mode of
  Position -> getVal pos seq
  Relative -> (getVal pos seq) + rel
  otherwise -> error "immediate mode for writing parameter doesn't work"

perform_opcode_1or2 com modes op = com { _sequence = M.insert write_at (fromEnum (v1 `op` v2)) seq ,
                                         _current_position = pos+4 }
                                   where
                                     seq = _sequence com
                                     pos = _current_position com
                                     write_mode = (modes ++ repeat Position) !! 2
                                     write_at = get_position_by_mode seq (pos+3) (_relative_base com) write_mode
                                     v1:v2:[] = get_values_by_mode com modes 2

perform_opcode_1 com modes = perform_opcode_1or2 com modes (+)
perform_opcode_2 com modes = perform_opcode_1or2 com modes (*)

perform_opcode_3 com modes =
  com { _sequence = M.insert write_at first_input seq ,
        _current_position = instr_pos+1,
        _inputs = other_inputs }
  where seq = _sequence com
        instr_pos = _current_position com + 1
        write_at = get_position_by_mode seq instr_pos (_relative_base com) (head modes)
        first_input:other_inputs = _inputs com

perform_opcode_4 com modes =
  com { _current_position=pos+2,
        _outputs=v:(_outputs com) }
  where pos = _current_position com
        seq = _sequence com
        v:[] = get_values_by_mode com modes 1


jump_if com modes cond = com { _sequence = seq,
                               _current_position = if (cond do_jump) then val else pos+3 }
                         where seq = _sequence com
                               pos = _current_position com
                               do_jump:val:[] = get_values_by_mode com modes 2

perform_opcode_5 com modes = jump_if com modes (/= 0)
perform_opcode_6 com modes = jump_if com modes (== 0)

perform_opcode_7 com modes = perform_opcode_1or2 com modes (<)
perform_opcode_8 com modes = perform_opcode_1or2 com modes (==)

perform_opcode_9 com modes =
  com { _relative_base = _relative_base com + v,
        _current_position = pos + 2 }
  where 
    pos = _current_position com
    v:[] = get_values_by_mode com modes 1

execute_current_opcode com =
  let seq = _sequence com in
  let pos = _current_position com in 
  let (instr, modes) = parse_opcode $ getVal pos seq in
  case instr of
    1         -> perform_opcode_1 com modes
    2         -> perform_opcode_2 com modes
    3         -> perform_opcode_3 com modes
    4         -> perform_opcode_4 com modes
    5         -> perform_opcode_5 com modes
    6         -> perform_opcode_6 com modes
    7         -> perform_opcode_7 com modes
    8         -> perform_opcode_8 com modes
    9         -> perform_opcode_9 com modes
    otherwise -> error ("No valid instruction " ++ (show instr) ++ " " ++ (show pos) ++ " " ++ (show $ seq))

iterate_program :: Computer -> Computer
iterate_program c =
  let s = _sequence c in
  let pos = _current_position c in
  let (op,_) = parse_opcode $ getVal pos s in
  case op of
    99 -> c { _state = Done }
    otherwise -> if (3 == op && _inputs c == [])
                 then c { _state = WaitingForInput }
                 else let new_c = execute_current_opcode c in
                      iterate_program $ new_c

run_program :: Computer -> Computer
run_program com = iterate_program com

runProgramWithInput :: Int -> Computer -> Computer
runProgramWithInput input computer =
  let c = computer { _inputs = input:(_inputs computer) } in
  run_program c

makeSequenceFrom :: [Int] -> M.Map Int Int
makeSequenceFrom = M.fromList . zip [0..]

main = do
  let memory_size = 10000
  let _seq = M.fromList $ zip [0..] ([109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99] ++ (replicate memory_size 0))
  let com = _default_computer { _sequence = _seq , _inputs = []}
  print $ run_program com
