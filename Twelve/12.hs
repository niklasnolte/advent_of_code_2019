import Data.Vector (V3)

data Moon = Moon {
  _getVelo :: V3,
  _getPos :: V3
} deriving (Show, Eq)


main = do
  let v1 = V3 1 2 3
  let v2 = V3 3 2 1
  print $ v1 + v2
