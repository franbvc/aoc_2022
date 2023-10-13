import Data.Char (ord)
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust)
import Data.Sequence qualified as Seq
import Data.Set qualified as S

type Position = (Int, Int)

lookupKey :: Eq v => v -> M.Map k v -> [k]
lookupKey val = M.foldrWithKey go []
  where
    go key value found =
      if value == val
        then key : found
        else found

convertToNumbers :: Char -> Int
convertToNumbers c
  | c == 'S' = 0
  | c == 'E' = 27
  | otherwise = (ord c) - 96

spotsAndPositions :: String -> (S.Set Position, M.Map Position Int)
spotsAndPositions text =
  foldr acc (S.empty, M.empty) $ do
    (y, line) <- zip [0 ..] $ lines text
    (x, cell) <- zip [0 ..] line
    [((x, y), cell)]
  where
    acc (pos, val) (spots, positions) =
      (S.insert pos spots, M.insert pos (convertToNumbers val) positions)

around :: S.Set Position -> Position -> [Position]
around valid (x, y) =
  filter (`S.member` valid) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

validNeighbors :: M.Map Position Int -> Position -> [Position] -> [Position]
validNeighbors positions currPos possibleNeighbors =
  [xs | (True, xs) <- zip isValid possibleNeighbors]
  where
    currVal = fromJust $ M.lookup currPos positions
    neighborsVal = map (\x -> fromJust $ M.lookup x positions) possibleNeighbors
    diff = map (\x -> x - currVal) neighborsVal
    isValid = map (<= 1) diff

dist :: (S.Set Position, M.Map Position Int) -> Position -> Position -> Maybe Int
dist (spots, positions) src dest = do
  search (S.singleton src) (Seq.singleton (src, 0)) dest positions
  where
    search visited toVisit sought posMap =
      case Seq.viewl toVisit of
        Seq.EmptyL -> Nothing
        (currPos, steps) Seq.:< rest ->
          let neighbors = validNeighbors posMap currPos (around (spots S.\\ visited) currPos)
              visited' = S.union visited (S.fromList neighbors)
              moreToTry = Seq.fromList $ map (\x -> (x, steps + 1)) neighbors
           in if currPos == sought
                then Just steps
                else search visited' (rest Seq.>< moreToTry) dest posMap

main = do
  contents <- readFile "./adventofcode.com_2022_day_12_input.txt"

  let (spots, positions) = spotsAndPositions contents

  let src = head $ lookupKey 0 positions
  let dest = head $ lookupKey 27 positions

  print $ dist (spots, positions) src dest