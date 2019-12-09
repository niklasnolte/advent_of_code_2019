split ::  Int -> [Int]
split x = [ read [y] :: Int | y <- show x ]

rising :: Int -> Bool
rising input = all (==True) [ k <= l | (k,l) <- zip (init x) (tail x)] where x = split input

two_adjacent_same :: Int -> [Bool]
two_adjacent_same x = [ y == z  | (y,z) <- zip (init k) (tail k) ] where k = split x

only_two_adjacent_same :: Int -> [Bool]
only_two_adjacent_same x = [ not x && y && not z | (x,y,z) <- zip3 (init $ init k) (init $ tail k) (tail $ tail k) ] where k = two_adjacent_same x

two_same_then_different :: [Int] -> Bool
two_same_then_different x = x!!0 == x!!1 && x!!0 /= x!!2

first_two_same :: Int -> Bool
first_two_same x = two_same_then_different $ take 3 (split x)

last_two_same :: Int -> Bool
last_two_same x = two_same_then_different $ take 3 (reverse $ split x)

crit1 :: Int -> Bool
crit1 x = any (==True) (two_adjacent_same x) && rising x

crit2 :: Int -> Bool
crit2 x = (any (==True) (only_two_adjacent_same x) || first_two_same x || last_two_same x) && rising x

popcount l = sum $ map fromEnum l

num_meet crit x = popcount [ crit y | y <- x ]

my_nums = [138307..654504]

answer_1 = num_meet crit1 my_nums
answer_2 = num_meet crit2 my_nums

main = do { putStrLn ("Answer 1: " ++ (show $ answer_1));
            putStrLn ("Answer 2: " ++ (show $ answer_2)) }
