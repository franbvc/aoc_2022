import Data.Char as C
import Data.List as L
import Data.Set as S

removeThree :: [String] -> [String]
removeThree xs = iterate tail xs !! 3

takeThree :: [String] -> [String]
takeThree = L.take 3

getGroupItems :: [String] -> [Char] -> [Char]
getGroupItems xs acc
  | L.null xs = acc
  | otherwise = getGroupItems (removeThree xs) (acc ++ getListIntersection (takeThree xs))

getListIntersection :: [String] -> [Char]
getListIntersection = S.toList . foldr1 S.intersection . Prelude.map S.fromList

getItemPriority :: Char -> Int
getItemPriority x
  | isLower x = C.ord x - C.ord 'a' + 1
  | isUpper x = C.ord x - C.ord 'A' + 27
  | otherwise = error "Not a letter"

itemSum :: [Char] -> Int
itemSum = sum . Prelude.map getItemPriority

main = do
  contents <- getContents
  let groupItems = getGroupItems (lines contents) []
  print $ itemSum groupItems
