{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
import           Control.Monad (liftM)
import           Data.List     (permutations)

data Computer =
  Computer {
    _inputs           :: [Int],
    _outputs          :: [Int],
    _current_position :: Int,
    _sequence         :: [Int],
    _done             :: Bool
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
  let (op,_) = parse_opcode $ s!!pos
  case op of
    99 -> return c { _done = True }
    otherwise -> if (3 == op && _inputs c == [])
                 then computer
                 else do new_c <- execute_current_opcode computer
                         iterate_program $ return new_c

run_program :: Computer -> IO Computer
run_program com = iterate_program $ return com

_default_seq = [3,8,1001,8,10,8,105,1,0,0,21,34,47,72,81,102,183,264,345,426,99999,3,9,102,5,9,9,1001,9,3,9,4,9,99,3,9,101,4,9,9,1002,9,3,9,4,9,99,3,9,102,3,9,9,101,2,9,9,102,5,9,9,1001,9,3,9,1002,9,4,9,4,9,99,3,9,101,5,9,9,4,9,99,3,9,101,3,9,9,1002,9,5,9,101,4,9,9,102,2,9,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,99]

_default_computer = Computer { _sequence = _default_seq,
                               _inputs = [],
                               _outputs = [],
                               _current_position = 0,
                               _done = False }


run_amplifier :: IO Computer -> [Int] -> IO Computer
run_amplifier my_state inputs = do
  s <- my_state
  run_program s { _inputs = inputs ++ (_inputs s) }

initialize_amplifier :: Int -> IO Computer
initialize_amplifier phase_setting = do
  run_amplifier (return _default_computer) [phase_setting]


run_amplifier_chain :: IO [Computer] -> Int -> IO [Computer] {- (input, phase settings) -> (amplifier signal, States -}
run_amplifier_chain states i = do
  (first:rest) <- states
  let inputs = _outputs $ last rest
  new_state <- run_amplifier (return first) (if inputs == [] then [0] else inputs)
  let new_states = return (rest ++ [new_state])
  if (_done new_state && i `mod` 5 == 4) {- only stop at the last -}
    then new_states
    else run_amplifier_chain new_states (i+1)

get_thruster_signal :: [Int] -> IO Int
get_thruster_signal phase_settings = do
  let amplifiers = sequence $ map initialize_amplifier phase_settings
  done <- run_amplifier_chain amplifiers 0
  return $ head $ _outputs $ last done

get_answer phase_values = do
  let all_permutations = permutations phase_values
  all_signals <- sequence $ map get_thruster_signal all_permutations
  return $ maximum all_signals
  

main = do
  a1 <- get_answer [0..4]
  print a1
  a2 <- get_answer [5..9]
  print a2
