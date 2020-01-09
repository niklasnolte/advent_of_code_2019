import Input8 (input, width, height)
import qualified Data.List as L
import qualified Data.Maybe as M
import System.IO.Unsafe (unsafePerformIO)

count_number_in :: Int -> [Int] -> Int
count_number_in n arr = L.length $ filter (==n) arr


split_into_multiple :: Int -> [a] -> [[a]]
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

{- for part 2 -}

get_relevant_pixel :: [Int] -> Maybe Int
get_relevant_pixel stack = L.find (/=2) stack

get_every_nth :: Int -> [Int] -> [Int]
get_every_nth n lst = [m | (i,m) <- zip [0..] lst, i `mod` n == 0]

get_pixel_stacks_from_input :: Int -> [Int] -> [[Int]]
get_pixel_stacks_from_input layer_size input =
  output
  where {
    get_stack i = get_every_nth layer_size $ drop i input;
    output = map get_stack [0..layer_size-1];
  }

substitute_color :: Int -> String
substitute_color i
  | i == 0 = " "
  | i == 1 = "█"

get_answer_2 :: [Int] -> Int -> Int -> IO ()
get_answer_2 input width height = do
  let pixels = map (M.fromJust . get_relevant_pixel) $ get_pixel_stacks_from_input (width*height) input
  print $ length pixels
  let rows = split_into_multiple width $ map substitute_color pixels
  let rows_as_strings = map (L.intercalate "") rows
  let image_as_numbers = L.intercalate "\n" rows_as_strings
  putStrLn image_as_numbers

main = do
  print $ get_answer_1 input width height
  get_answer_2 input width height
