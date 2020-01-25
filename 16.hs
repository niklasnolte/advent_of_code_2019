import qualified Data.List as L

type Signal = [Int]
type Pattern = [Int]

basePattern = [0,1,0,-1]

patternFor :: Int -> Pattern -> Pattern
patternFor i p =
  tail $ cycle $ L.concat $ L.map (replicate (i+1)) p


applyPatternAt :: Int -> Signal -> Pattern -> Int
applyPatternAt i s p =
  let thisP = patternFor i p in
  (abs $ L.sum $ L.zipWith (*) s thisP) `mod` 10

applyPattern :: Signal -> Pattern -> Signal
applyPattern s p =
  let l = length s in
  [applyPatternAt idx s p | idx <- take l [0..]]

applyPatternForN :: Int -> Int -> Signal -> Pattern -> Signal
applyPatternForN n i s p = if n == i
  then s
  else let ns = applyPattern s p in
       applyPatternForN n (i+1) ns p

input = "59717238168580010599012527510943149347930742822899638247083005855483867484356055489419913512721095561655265107745972739464268846374728393507509840854109803718802780543298141398644955506149914796775885246602123746866223528356493012136152974218720542297275145465188153752865061822191530129420866198952553101979463026278788735726652297857883278524565751999458902550203666358043355816162788135488915722989560163456057551268306318085020948544474108340969874943659788076333934419729831896081431886621996610143785624166789772013707177940150230042563041915624525900826097730790562543352690091653041839771125119162154625459654861922989186784414455453132011498"

parseSignal :: String -> [Int]
parseSignal = map ((read :: String -> Int) . (:[]))

main = do
  let s = parseSignal input
  let sAfter100 = applyPatternForN 100 0 s basePattern
  print $ take 8 sAfter100
