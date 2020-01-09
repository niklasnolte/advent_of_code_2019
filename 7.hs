{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
import           Control.Monad (liftM)
import           Data.List     (permutations)
import Intcode

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
