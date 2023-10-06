import Data.Char as C
import Data.List as L
import Data.Set as S

middleSplit :: [a] -> ([a], [a])
middleSplit xs = L.splitAt (length xs `div` 2) xs   

getIntersection :: Ord a => ([a], [a]) -> Set a
getIntersection (xs, ys) = S.fromList xs `S.intersection` S.fromList ys
-- convertToSet (xs, ys) = (Data.Set.fromList xs, Data.Set.fromList ys)

getItemPriority :: Char -> Int
getItemPriority x
  | isLower x = C.ord x - C.ord 'a' + 1 
  | isUpper x = C.ord x - C.ord 'A' + 27
  | otherwise = error "Not a letter"

intersectionSum :: Set Char -> Int
intersectionSum = S.foldr (\x acc -> acc + getItemPriority x) 0

main = do
  contents <- getContents
  let dividedList = [middleSplit line | line <- lines contents]
  let setList = [getIntersection line | line <- dividedList]

  print $ sum $ Prelude.map intersectionSum setList
