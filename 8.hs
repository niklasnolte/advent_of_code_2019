import Input8 (input, width, height)
import qualified Data.List as L
import qualified Data.Maybe as M

count_number_in :: Int -> [Int] -> Int
count_number_in n arr = L.length $ filter (==n) arr


split_into_multiple :: Int -> [Int] -> [[Int]]
split_into_multiple n input = let len = L.length input in [take n $ drop x input | x <- [0,n..len-1]]

get_layer_with_least_zeros :: [[Int]] -> [Int]
get_layer_with_least_zeros input =
  output
  where {
    counts = map (count_number_in 0) input;
    idx = M.fromJust $ L.findIndex (== (minimum counts)) counts;
    output = input!!idx;
  }

get_answer_1 :: [Int] -> Int -> Int -> Int
get_answer_1 input width height =
  n_ones * n_twos
  where {
    layers = split_into_multiple (width*height) input;
    least_zero_layer = get_layer_with_least_zeros layers;
    n_ones = count_number_in 1 least_zero_layer;
    n_twos = count_number_in 2 least_zero_layer;
  }


test_count_number_in:: IO ()
test_count_number_in = print $ count_number_in 0 [0,1,0,2]

test_split_into_multiple :: IO ()
test_split_into_multiple = do print $ split_into_multiple 3 [1..9]
                              print $ split_into_multiple 3 [0..9]

test_get_layer_with_least_zeros :: IO ()
test_get_layer_with_least_zeros = do
  let input = [[0,1,2,0], [2,1,0,0], [4,0,1,1]]
  print $ get_layer_with_least_zeros input

main = do
  print $ get_answer_1 input width height
