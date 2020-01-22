import qualified Data.Map as M
import qualified Data.List as L
import Data.Ratio ((%))
import Data.Maybe (fromJust)
import Debug.Trace (trace)

type Amount = Int
type LeftOver = Int
type Chemical = String
type Ingredient = (Chemical, Amount)
type Reactions = M.Map Chemical ([Ingredient], Amount)
type ReactionsEntry = (Chemical, ([Ingredient], Amount))
type IngredientMap = M.Map Chemical (Amount, LeftOver)

isDigit x = x `elem` ['0'..'9']
isLetter x = x `elem` ['A'..'Z']

extractIngredient :: String -> Ingredient -> (String, Ingredient)
extractIngredient "" result = ("", result)
extractIngredient s@(c:others) (ch, am)
  | c == ' ' = extractIngredient others (ch, am)
  | isDigit c = let n = read (takeWhile isDigit s) :: Int in
                let rest = dropWhile isDigit s in
                extractIngredient rest (ch, n)
  | isLetter c = let newCh = takeWhile isLetter s in
                 let rest = dropWhile isLetter s in
                 (rest,(newCh, am))


toReaction :: String -> ReactionsEntry -> ReactionsEntry
toReaction s@(c:others) entry@(ch, (ings, am))
  | c == ' ' || c == ',' = toReaction others entry
  | c /= '=' = let (rest, newIng) = extractIngredient s ("", 0) in
               toReaction rest (ch, (newIng:ings, am))
  | otherwise = let (_, (newCh, newAm)) = extractIngredient (drop 2 others) ("",0) in
                (newCh, (ings, newAm))


addAmountsAndLeftOvers :: (Amount, LeftOver) -> (Amount, LeftOver) -> (Amount, LeftOver)
addAmountsAndLeftOvers (anew,lnew) (aold, lold) =
  let resA = anew+aold-lnew-lold in
  if resA < 0
    then (0,-resA)
    else (resA, 0)

requiredIngredientsFor :: Ingredient -> Reactions -> IngredientMap
requiredIngredientsFor ing@(chemical, amount) reactions =
  let ingsOrNothing = M.lookup chemical reactions in
  case ingsOrNothing of
  Nothing -> M.singleton chemical (amount,0)
  _ -> let (ingsForOne, nProducedInOneReaction) = fromJust ingsOrNothing in
       let nReactionsNeeded = ceiling $ amount % nProducedInOneReaction in
       let nLeftOver = (nReactionsNeeded * nProducedInOneReaction) - amount in
       let ingsForAmount = L.map (\(ch,am) -> (ch, (am*nReactionsNeeded, 0))) ingsForOne in
       let requiredIngs = M.fromList ingsForAmount in
       M.insertWith addAmountsAndLeftOvers chemical (0, nLeftOver) requiredIngs

findNonOreChemical :: IngredientMap -> Maybe (Chemical, (Amount, LeftOver))
findNonOreChemical map = 
  let ingList = M.assocs map in
  L.find (\(ch,(am,_)) -> (ch /= "ORE") && (am > 0)) ingList

countOreFor :: IngredientMap -> Reactions -> Int
countOreFor myIngredients reactions =
  let newChemicalOrNothing = findNonOreChemical myIngredients in
  case newChemicalOrNothing of
  Nothing -> fromJust $ L.find (/=0) $ map fst $ M.elems myIngredients
  _ -> let (newChemical, (howMany,_)) = fromJust newChemicalOrNothing in
       let reqIngs = requiredIngredientsFor (newChemical, howMany) reactions in
       let myIngredientsWithoutChem = M.insertWith addAmountsAndLeftOvers
                                                   newChemical
                                                   (-howMany, 0)
                                                   myIngredients in
       let newIngredients = M.unionWith addAmountsAndLeftOvers reqIngs myIngredientsWithoutChem in
       countOreFor newIngredients reactions

countOreToProduceFuel :: Int -> [String] -> Int
countOreToProduceFuel n input =
  let reactions = M.fromList $ map (`toReaction` ("", ([], 0))) input in
  countOreFor (M.singleton "FUEL" (n,0)) reactions

countFuelForGivenOre :: Int -> [String] -> Int
countFuelForGivenOre givenOre input =
  dispatch 1
  where countFor x = countOreToProduceFuel x input
        dispatch estimate =
          let c = countFor estimate in
          if countFor (estimate + 1) > givenOre && countFor estimate < givenOre
          then estimate
          else let diff = givenOre - c in
               let m = c `div` estimate in
               let newEstimate = estimate + (diff  `div` m + signum diff ) in
               dispatch $ newEstimate


input = [
  "4 JWXL => 8 SNBF",
  "23 MPZQF, 10 TXVW, 8 JWXL => 6 DXLB",
  "1 SNZDR, 5 XMWHC, 1 NJSC => 7 MHSB",
  "2 TDHD, 11 TXVW => 4 RFNZ",
  "2 VRCD, 1 FGZG, 3 JWXL, 1 HQTL, 2 MPZQF, 1 GTPJ, 5 HQNMK, 10 CQZQ => 9 QMTZB",
  "3 SRDB, 2 ZMVLP => 3 DHFD",
  "1 DFQGF => 1 CVXJR",
  "193 ORE => 3 TRWXF",
  "23 MFJMS, 4 HJXJH => 1 WVDF",
  "5 TRWXF => 5 RXFJ",
  "4 GZQH => 7 SNZDR",
  "160 ORE => 4 PLPF",
  "1 PLPF => 5 NJSC",
  "2 QKPZ, 2 JBWFL => 7 HBSC",
  "15 DXLB, 1 TDHD, 9 RFNZ => 5 DBRPW",
  "7 PLPF, 4 GMZH => 7 PVNX",
  "3 JWXL, 1 XWDNT, 4 CQZQ => 2 TPBXV",
  "2 SNZDR => 9 WQWT",
  "1 WMCF => 2 XWDNT",
  "1 DFQGF, 8 FGZG => 5 LMHJQ",
  "168 ORE => 9 GMZH",
  "18 PVNX, 3 RXFJ => 4 JBWFL",
  "5 WQWT => 1 CQZQ",
  "6 QMTZB, 28 NVWM, 8 LMHJQ, 1 SNBF, 15 PLPF, 3 KMXPQ, 43 WVDF, 52 SVNS => 1 FUEL",
  "164 ORE => 9 RXRMQ",
  "2 MFJMS, 1 HJXJH, 7 WVDF => 7 NXWC",
  "8 QDGBV, 1 WMCF, 2 MHSB => 6 HQTL",
  "1 XMWHC => 8 MLSK",
  "2 GMZH, 1 RXRMQ => 2 GZQH",
  "4 MPZQF, 7 WVDF => 9 KHJMV",
  "4 ZMVLP, 19 MLSK, 1 GZQH => 8 MFJMS",
  "1 HQTL, 1 SXKQ => 2 PWBKR",
  "3 SXKQ, 16 TXVW, 4 SVNS => 5 PSRF",
  "4 MPZQF, 3 SVNS => 9 QDGBV",
  "7 NXWC => 8 FGZG",
  "7 TDHD, 1 WQWT, 1 HBSC => 9 TXVW",
  "14 JBWFL => 5 LMXB",
  "1 VRCD, 3 KHJMV => 3 RTBL",
  "16 DHFD, 2 LBNK => 9 SXKQ",
  "1 QDGBV, 1 NJSC => 6 JWXL",
  "4 KHJMV => 3 HQNMK",
  "5 GZQH => 6 LBNK",
  "12 KHJMV, 19 FGZG, 3 XWDNT => 4 VRCD",
  "5 DHFD, 3 MLSK => 8 QKPZ",
  "4 KHJMV, 1 CQDR, 3 DBRPW, 2 CQZQ, 1 TPBXV, 15 TXVW, 2 TKSLM => 5 NVWM",
  "2 KHJMV => 5 CQDR",
  "1 CVXJR => 8 SVNS",
  "35 RXFJ, 5 NJSC, 22 PVNX => 9 HJXJH",
  "5 LMXB => 3 DFQGF",
  "1 RXFJ => 2 SRDB",
  "20 TPBXV, 1 RTBL, 13 PWBKR, 6 RFNZ, 1 LMXB, 2 CVXJR, 3 PSRF, 25 MPZQF => 9 KMXPQ",
  "1 MHSB, 8 MPZQF => 3 TDHD",
  "6 DHFD, 3 LBNK => 7 WMCF",
  "1 SRDB => 7 ZMVLP",
  "3 RXFJ => 8 XMWHC",
  "1 MPZQF => 8 TKSLM",
  "9 JBWFL, 22 WQWT => 8 MPZQF",
  "12 HBSC, 15 TKSLM => 1 GTPJ"
  ]

main = do
  print $ countOreToProduceFuel 1 input
  let givenOre = 1000000000000
  print $ countFuelForGivenOre givenOre input
