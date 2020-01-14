module Intcode where
import System.IO.Unsafe (unsafePerformIO)

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

data State = Running | WaitingForInput | Done deriving (Enum, Show, Eq)

data Computer =
  Computer {
    _inputs           :: [Int],
    _outputs          :: [Int],
    _current_position :: Int,
    _sequence         :: [Int],
    _state            :: State,
    _relative_base    :: Int
  } deriving (Show)

_default_computer = Computer { _inputs = [],
                               _outputs = [],
                               _current_position = 0,
                               _sequence = [],
                               _state = Running,
                               _relative_base = 0 }

data Mode = Position | Immediate | Relative deriving (Enum, Show, Eq)

parse_opcode opcode = (op, modes)
                      where {
                        op = opcode `mod` 100;
                        modes_as_int =  map read $ reverse $ map (:[]) $ show $ opcode `div` 100 ::[Int];
                        modes = map (\x -> toEnum x :: Mode) modes_as_int;
                      }

modify_sequence pos seq value = take pos seq ++ [value] ++ drop (pos+1) seq

get_values_by_mode com modes n = unsafePerformIO $ do
  return [ get_val v mode | (v,mode) <- zip (take n $ drop start seq) (modes ++ repeat Position) ]
  where start = _current_position com + 1
        seq = _sequence com
        get_val v mode = case mode of
          Position -> seq!!v
          Immediate -> v
          Relative -> seq!!(v + _relative_base com)

get_position_by_mode seq pos rel mode = case mode of
  Position -> seq!!pos
  Relative -> seq!!pos + rel
  otherwise -> error "immediate mode for writing parameter doesn't work"

perform_opcode_1or2 com modes op = com { _sequence = modify_sequence write_at seq $ fromEnum (v1 `op` v2),
                                         _current_position = pos+4 }
                                   where
                                     seq = _sequence com
                                     pos = _current_position com
                                     write_mode = (modes ++ repeat Position) !! 2
                                     write_at = get_position_by_mode seq (pos+3) (_relative_base com) write_mode
                                     v1:v2:[] = get_values_by_mode com modes 2

perform_opcode_1 com modes = perform_opcode_1or2 com modes (+)
perform_opcode_2 com modes = perform_opcode_1or2 com modes (*)

perform_opcode_3 com modes = do
  return com { _sequence = modify_sequence write_at seq first_input,
               _current_position = instr_pos+1,
               _inputs = other_inputs }
  where seq = _sequence com
        instr_pos = _current_position com + 1
        write_at = get_position_by_mode seq instr_pos (_relative_base com) (head modes)
        first_input:other_inputs = _inputs com

perform_opcode_4 com modes = do
  return com { _current_position=pos+2,
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

execute_current_opcode computer = do
  com <- computer
  let seq = _sequence com
  let pos = _current_position com
  let (instr, modes) = parse_opcode $ seq!!pos
  case instr of
    1         -> return $ perform_opcode_1 com modes
    2         -> return $ perform_opcode_2 com modes
    3         -> perform_opcode_3 com modes
    4         -> perform_opcode_4 com modes
    5         -> return $ perform_opcode_5 com modes
    6         -> return $ perform_opcode_6 com modes
    7         -> return $ perform_opcode_7 com modes
    8         -> return $ perform_opcode_8 com modes
    9         -> return $ perform_opcode_9 com modes
    otherwise -> error ("No valid instruction " ++ (show instr) ++ " " ++ (show pos) ++ " " ++ (show $ seq))

iterate_program :: IO Computer -> IO Computer
iterate_program computer = do
  c <- computer
  let s = _sequence c
  let pos = _current_position c
  let (op,_) = parse_opcode $ s!!pos
  case op of
    99 -> return c { _state = Done }
    otherwise -> if (3 == op && _inputs c == [])
                 then return c { _state = WaitingForInput }
                 else do new_c <- execute_current_opcode computer
                         iterate_program $ return new_c

run_program :: Computer -> IO Computer
run_program com = iterate_program $ return com

runProgramWithInput :: Int -> Computer -> IO Computer
runProgramWithInput input computer = do
  let c = computer { _inputs = input:(_inputs computer) }
  run_program c

main = do
  let memory_size = 10000
  let _seq = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99] ++ (take memory_size $ repeat 0)
  let com = _default_computer { _sequence = _seq , _inputs = []}
  run_program com >>= print
