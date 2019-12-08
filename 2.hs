import Data.List
import Data.Maybe


input = [1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,10,19,1,19,6,23,2,23,13,27,1,27,5,31,2,31,10,35,1,9,35,39,1,39,9,43,2,9,43,47,1,5,47,51,2,13,51,55,1,55,9,59,2,6,59,63,1,63,5,67,1,10,67,71,1,71,10,75,2,75,13,79,2,79,13,83,1,5,83,87,1,87,6,91,2,91,13,95,1,5,95,99,1,99,2,103,1,103,6,0,99,2,14,0,0]

change_element_in_seq pos seq value = [ if idx == pos then value else x | (x,idx) <- zip seq [0..] ]


perform_opcode pos seq op = change_element_in_seq pos_to_write seq ((seq!!v1) `op` (seq!!v2))
                             where v1:v2:pos_to_write:_ = drop (pos+1) seq

perform_opcode_1 pos seq = perform_opcode pos seq (+)
perform_opcode_2 pos seq = perform_opcode pos seq (*)

execute_opcode_at pos seq
                       | instr == 1 = perform_opcode_1 pos seq
                       | instr == 2 = perform_opcode_2 pos seq
                       | otherwise  = error "No valid instruction"
                       where instr = seq!!pos

iterate_over_sequence pos seq = if seq!!pos == 99 {-breaking condition-}
                                then seq
                                else iterate_over_sequence (pos+4) (execute_opcode_at pos seq)

initialize_sequence noun verb = (head input):noun:verb:(drop 3 input)

get_answer_for x y = head $ iterate_over_sequence 0 (initialize_sequence x y)

{- part 1 -}
answer_1 = get_answer_for 12 2

{-part 2-}
target_answer = 19690720 :: Int


all_answers = [ (get_answer_for x y,(x,y)) | x <- [1..99], y <- [1..99] ]

answer_2 = lookup target_answer all_answers

main = do
       putStrLn ("Answer for part 1: " ++ show answer_1)
       putStrLn ("Answer for part 2: " ++ (show . fromJust) answer_2)
