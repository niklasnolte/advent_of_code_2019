import qualified Data.List as L

type Signal = [Int]
type Pattern = [Int]


patternFor :: Int -> Pattern
patternFor i =
  let basePattern = [0,1,0,-1] in
  tail $
  cycle $
  L.concat $
  L.map (replicate (i+1)) basePattern


applyPatternAt :: Int -> Signal -> Int
applyPatternAt i s =
  let thisP = patternFor i in
  (abs $ L.sum $ L.zipWith (*) s thisP) `mod` 10

applyPattern :: Signal -> Signal
applyPattern s =
  let l = length s in
  [applyPatternAt idx s | idx <- take l [0..]]

applyPatternForN :: Int -> Signal -> Signal
applyPatternForN n s = if n == 0
  then s
  else let ns = applyPattern s in
       applyPatternForN (n-1) ns

applyPatternForNUpperHalf :: Int -> Signal -> Signal
applyPatternForNUpperHalf 0 s = s
applyPatternForNUpperHalf n s =
  let ns = map (`mod` 10) $ scanr1 (+) s in
  applyPatternForNUpperHalf (n-1) ns

testInput = "03036732577212944063491565474664"

input = "59717238168580010599012527510943149347930742822899638247083005855483867484356055489419913512721095561655265107745972739464268846374728393507509840854109803718802780543298141398644955506149914796775885246602123746866223528356493012136152974218720542297275145465188153752865061822191530129420866198952553101979463026278788735726652297857883278524565751999458902550203666358043355816162788135488915722989560163456057551268306318085020948544474108340969874943659788076333934419729831896081431886621996610143785624166789772013707177940150230042563041915624525900826097730790562543352690091653041839771125119162154625459654861922989186784414455453132011498"

parseSignal :: String -> [Int]
parseSignal = map ((read :: String -> Int) . (:[]))

main :: IO()
main = do
  let s = parseSignal input
  let sAfter100 = applyPatternForN 100 s
  print $ take 8 sAfter100
  putStrLn "part2"
  let s = L.concat $ L.replicate 10000 $ parseSignal input
  let idx = read (take 7 input) :: Int
  let newS = drop idx s --idx needs to be bigger than ((length s)/2) for this to work
  let newSAfter100 = applyPatternForNUpperHalf 100 newS
  print $ take 8 $ newSAfter100


