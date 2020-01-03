{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
import           Control.Monad (liftM)
import           Data.List     (permutations)

data Computer =
  Computer {
    _inputs           :: [Int],
    _outputs          :: [Int],
    _current_position :: Int,
    _sequence         :: [Int]
  } deriving (Show)


parse_opcode opcode = (op, modes)
                      where {
                        op = opcode `mod` 100;
                        modes = map read $ reverse $ map (:[]) $ show $ opcode `div` 100 ::[Int]
                      }

modify_sequence pos seq value = take pos seq ++ [value] ++ drop (pos+1) seq

get_values_by_mode start seq modes n = [ if mode == 0 then (seq!!v) else v |
                                        (v,mode) <- zip (take n $ drop start seq) (modes ++ [0,0..]) ]

perform_opcode_1or2 pos seq modes op = (modify_sequence (seq!!(pos+3)) seq $ fromEnum (v1 `op` v2), pos+4)
                                        where v1:v2:[] = get_values_by_mode (pos+1) seq modes 2

perform_opcode_1 pos seq modes = perform_opcode_1or2 pos seq modes (+)
perform_opcode_2 pos seq modes = perform_opcode_1or2 pos seq modes (*)

perform_opcode_3 com = return com { _sequence = modify_sequence (seq!!(pos+1)) seq first_input,
                                    _current_position = pos+2,
                                    _inputs = other_inputs }
                       where seq = _sequence com
                             pos = _current_position com
                             first_input:other_inputs = _inputs com

perform_opcode_4 com modes = do { return com { _current_position=pos+2,
                                               _outputs=v:(_outputs com) }
                                } where pos = _current_position com
                                        seq = _sequence com
                                        v:[] = get_values_by_mode (pos+1) seq modes 1


jump_if pos seq modes cond = (seq, if (cond do_jump) then val else pos+3)
                             where do_jump:val:[] = get_values_by_mode (pos+1) seq modes 2

perform_opcode_5 pos seq modes = jump_if pos seq modes (/= 0)
perform_opcode_6 pos seq modes = jump_if pos seq modes (== 0)

perform_opcode_7 pos seq modes = perform_opcode_1or2 pos seq modes (<)
perform_opcode_8 pos seq modes = perform_opcode_1or2 pos seq modes (==)

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
    3         -> perform_opcode_3 com
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
  if (let (op,_) = parse_opcode $ s!!pos in 99 == op) {-breaking condition-}
  then computer
  else do new_c <- execute_current_opcode computer
          iterate_program $ return new_c

run_program :: Computer -> IO Computer
run_program com = iterate_program $ return com

_default_seq = [3,8,1001,8,10,8,105,1,0,0,21,34,47,72,81,102,183,264,345,426,99999,3,9,102,5,9,9,1001,9,3,9,4,9,99,3,9,101,4,9,9,1002,9,3,9,4,9,99,3,9,102,3,9,9,101,2,9,9,102,5,9,9,1001,9,3,9,1002,9,4,9,4,9,99,3,9,101,5,9,9,4,9,99,3,9,101,3,9,9,1002,9,5,9,101,4,9,9,102,2,9,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,99]

_default_computer = Computer { _sequence = _default_seq,
                               _inputs = [],
                               _outputs = [],
                               _current_position = 0 }


link_thrusters :: IO Computer -> Int -> IO Computer
link_thrusters lhs phase_setting = do
  old <- lhs
  let output:[] = _outputs old
  let rhs =  _default_computer { _inputs = [phase_setting, output]}
  run_program rhs


main = do
  let _all_permutations = permutations [4,3,2,1,0]
  let get_output x = x >>= return . head . _outputs
  all_outputs <- sequence $ map (get_output . foldl link_thrusters (return _default_computer {_outputs = [0]})) _all_permutations
  print $ maximum all_outputs
