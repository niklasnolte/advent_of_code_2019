module Intcode where

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

relative_base = 0

data Computer =
  Computer {
    _inputs           :: [Int],
    _outputs          :: [Int],
    _current_position :: Int,
    _sequence         :: [Int],
    _done             :: Bool
  } deriving (Show)

data Mode = Position | Immediate | Relative deriving (Enum, Show, Eq)

parse_opcode opcode = (op, modes)
                      where {
                        op = opcode `mod` 100;
                        modes_as_int =  map read $ reverse $ map (:[]) $ show $ opcode `div` 100 ::[Int];
                        modes = map (\x -> toEnum x :: Mode) modes_as_int;
                      }

modify_sequence pos seq value = take pos seq ++ [value] ++ drop (pos+1) seq

get_values_by_mode start seq modes n =
  [ get_val v mode | (v,mode) <- zip (take n $ drop start seq) (modes ++ repeat Position) ]
  where get_val v mode
          | mode == Position = seq!!v
          | mode == Immediate = v
          | mode == Relative = seq!!(v + relative_base)

perform_opcode_1or2 pos seq modes op = (modify_sequence (seq!!(pos+3)) seq $ fromEnum (v1 `op` v2), pos+4)
                                        where v1:v2:[] = get_values_by_mode (pos+1) seq modes 2

perform_opcode_1 pos seq modes = perform_opcode_1or2 pos seq modes (+)
perform_opcode_2 pos seq modes = perform_opcode_1or2 pos seq modes (*)

perform_opcode_3 com modes = do
  return com { _sequence = modify_sequence (seq !! (pos+1)) seq first_input,
               _current_position = pos+2,
               _inputs = other_inputs }
  where seq = _sequence com
        pos = _current_position com
        val:[] = get_values_by_mode (pos+1) seq modes 1
        first_input:other_inputs = _inputs com

perform_opcode_4 com modes = do { return com { _current_position=pos+2,
                                               _outputs=[v] }
                                } where pos = _current_position com
                                        seq = _sequence com
                                        v:[] = get_values_by_mode (pos+1) seq modes 1


jump_if pos seq modes cond = (seq, if (cond do_jump) then val else pos+3)
                             where do_jump:val:[] = get_values_by_mode (pos+1) seq modes 2

perform_opcode_5 pos seq modes = jump_if pos seq modes (/= 0)
perform_opcode_6 pos seq modes = jump_if pos seq modes (== 0)

perform_opcode_7 pos seq modes = perform_opcode_1or2 pos seq modes (<)
perform_opcode_8 pos seq modes = perform_opcode_1or2 pos seq modes (==)

{-perform_opcode_9 pos seq modes =-}

execute_current_opcode computer = do
  com <- computer
  let seq = _sequence com
  let pos = _current_position com
  let (instr, modes) = parse_opcode $ seq!!pos

  let non_io_action = (\opcode_action -> do
                         let (_seq,_pos) = opcode_action pos seq modes
                         return com { _sequence=_seq, _current_position=_pos } )

  case instr of
    1         -> non_io_action perform_opcode_1
    2         -> non_io_action perform_opcode_2
    3         -> perform_opcode_3 com modes
    4         -> perform_opcode_4 com modes
    5         -> non_io_action perform_opcode_5
    6         -> non_io_action perform_opcode_6
    7         -> non_io_action perform_opcode_7
    8         -> non_io_action perform_opcode_8
    otherwise -> error ("No valid instruction " ++ (show instr))

iterate_program :: IO Computer -> IO Computer
iterate_program computer = do
  c <- computer
  let s = _sequence c
  let pos = _current_position c
  let (op,_) = parse_opcode $ s!!pos
  case op of
    99 -> return c { _done = True }
    otherwise -> if (3 == op && _inputs c == [])
                 then computer
                 else do new_c <- execute_current_opcode computer
                         iterate_program $ return new_c

run_program :: Computer -> IO Computer
run_program com = iterate_program $ return com
