import Data.List
import Data.Maybe
import Control.Monad

parse_input = do
       x <- getLine
       return (read x :: Int)

parse_opcode opcode = (op, modes)
                      where {
                        op = opcode `mod` 100;
                        modes = map read $ reverse $ map (:[]) $ show $ opcode `div` 100 ::[Int]
                      }



change_element_in_seq pos seq value = [ if idx == pos then value else x | (x,idx) <- zip seq [0..] ]

get_value_by_mode start seq modes n = [ if mode == 0 then (seq!!v) else v |
                                        (v,idx,mode) <- zip3 (take n $ drop start seq) [0..] (modes ++ [0,0..]) ]

perform_opcode_1or2 pos seq modes op = (change_element_in_seq (seq!!(pos+3)) seq $ fromEnum (v1 `op` v2), pos+4)
                                        where v1:v2:[] = get_value_by_mode (pos+1) seq modes 2

perform_opcode_1 pos seq modes = perform_opcode_1or2 pos seq modes (+)
perform_opcode_2 pos seq modes = perform_opcode_1or2 pos seq modes (*)

perform_opcode_3 pos seq = do { val <- parse_input; return $ (change_element_in_seq (seq!!(pos+1)) seq val, pos+2) }

perform_opcode_4 pos seq modes = do { putStrLn $ show $ v;
                                      return (seq, pos+2); }
                                      where v = head $ get_value_by_mode (pos+1) seq modes 1

jump_if pos seq modes cond = (seq, if (cond do_jump) then val else pos+3)
                             where do_jump:val:[] = get_value_by_mode (pos+1) seq modes 2

perform_opcode_5 pos seq modes = jump_if pos seq modes (/= 0)
perform_opcode_6 pos seq modes = jump_if pos seq modes (== 0)

perform_opcode_7 pos seq modes = perform_opcode_1or2 pos seq modes (<)
perform_opcode_8 pos seq modes = perform_opcode_1or2 pos seq modes (==)

execute_opcode_at pos seq = do let (instr, modes) = parse_opcode $ seq!!pos;
                               case instr of
                                 1 -> return $ perform_opcode_1 pos seq modes
                                 2 -> return $ perform_opcode_2 pos seq modes
                                 3 -> perform_opcode_3 pos seq
                                 4 -> perform_opcode_4 pos seq modes
                                 5 -> return $ perform_opcode_5 pos seq modes
                                 6 -> return $ perform_opcode_6 pos seq modes
                                 7 -> return $ perform_opcode_7 pos seq modes
                                 8 -> return $ perform_opcode_8 pos seq modes
                                 otherwise -> error ("No valid instruction " ++ (show instr))

iterate_over_sequence pos seq = do s <- seq
                                   if (let (op,_) = parse_opcode $ s!!pos in 99 == op) {-breaking condition-}
                                   then return s
                                   else do (ret, curr_instr) <- execute_opcode_at pos s
                                           {-putStrLn $ show curr_instr-}
                                           iterate_over_sequence curr_instr $ return ret


__seq = [3,225,1,225,6,6,1100,1,238,225,104,0,1001,191,50,224,101,-64,224,224,4,224,1002,223,8,223,101,5,224,224,1,224,223,223,2,150,218,224,1001,224,-1537,224,4,224,102,8,223,223,1001,224,2,224,1,223,224,223,1002,154,5,224,101,-35,224,224,4,224,1002,223,8,223,1001,224,5,224,1,224,223,223,1102,76,17,225,1102,21,44,224,1001,224,-924,224,4,224,102,8,223,223,1001,224,4,224,1,224,223,223,101,37,161,224,101,-70,224,224,4,224,1002,223,8,223,101,6,224,224,1,223,224,223,102,46,157,224,1001,224,-1978,224,4,224,102,8,223,223,1001,224,5,224,1,224,223,223,1102,5,29,225,1101,10,7,225,1101,43,38,225,1102,33,46,225,1,80,188,224,1001,224,-73,224,4,224,102,8,223,223,101,4,224,224,1,224,223,223,1101,52,56,225,1101,14,22,225,1101,66,49,224,1001,224,-115,224,4,224,1002,223,8,223,1001,224,7,224,1,224,223,223,1101,25,53,225,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,108,226,226,224,1002,223,2,223,1005,224,329,101,1,223,223,108,677,677,224,1002,223,2,223,1006,224,344,1001,223,1,223,8,677,677,224,102,2,223,223,1006,224,359,101,1,223,223,7,226,677,224,102,2,223,223,1005,224,374,101,1,223,223,107,226,226,224,102,2,223,223,1006,224,389,101,1,223,223,7,677,226,224,1002,223,2,223,1006,224,404,1001,223,1,223,1107,677,226,224,1002,223,2,223,1006,224,419,1001,223,1,223,1007,226,226,224,102,2,223,223,1005,224,434,101,1,223,223,1008,226,677,224,102,2,223,223,1005,224,449,1001,223,1,223,1007,677,677,224,1002,223,2,223,1006,224,464,1001,223,1,223,1008,226,226,224,102,2,223,223,1006,224,479,101,1,223,223,1007,226,677,224,1002,223,2,223,1005,224,494,1001,223,1,223,108,226,677,224,1002,223,2,223,1006,224,509,101,1,223,223,8,226,677,224,102,2,223,223,1005,224,524,1001,223,1,223,107,677,677,224,1002,223,2,223,1005,224,539,101,1,223,223,107,226,677,224,1002,223,2,223,1006,224,554,101,1,223,223,1107,226,677,224,1002,223,2,223,1006,224,569,1001,223,1,223,1108,677,226,224,102,2,223,223,1005,224,584,1001,223,1,223,1008,677,677,224,102,2,223,223,1005,224,599,1001,223,1,223,1107,677,677,224,102,2,223,223,1006,224,614,101,1,223,223,7,226,226,224,102,2,223,223,1005,224,629,1001,223,1,223,1108,677,677,224,102,2,223,223,1006,224,644,1001,223,1,223,8,677,226,224,1002,223,2,223,1005,224,659,101,1,223,223,1108,226,677,224,102,2,223,223,1005,224,674,101,1,223,223,4,223,99,226]

initialize_sequence noun verb = (head input):noun:verb:(drop 3 input)
                                 where input = __seq

get_answer_for x y = (iterate_over_sequence 0 $ return $ initialize_sequence x y) >>= (\z ->return $ head z)

get_answer seq = (iterate_over_sequence 0 $ return $ seq) >>= (\z ->return z)

main = do
       answer_1 <- get_answer __seq
       putStrLn ("Answer for part 1: " ++ show answer_1) {- put in 1 -}
       answer_2 <- get_answer __seq
       putStrLn ("Answer for part 2: " ++ show answer_2) {- put in 5 -}
