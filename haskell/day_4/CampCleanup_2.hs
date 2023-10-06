import Data.Char as C
import Data.List as L

getNextNumber :: String -> (String, Int)
getNextNumber xs = do
  let nextNumber = takeWhile isNumber xs
  let numberLength = length nextNumber
  (drop numberLength xs, read nextNumber :: Int)

splitNumbers :: String -> [Int] -> [Int]
splitNumbers xs acc
  | null xs = acc
  | not . isNumber . head $ xs = splitNumbers (tail xs) acc
  | otherwise = splitNumbers (fst nextNumber) (acc ++ [snd nextNumber])
  where
    nextNumber = getNextNumber xs

isOverlap :: [Int] -> Bool
isOverlap xs = not ((xs !! 1 < xs !! 2) || (xs !! 3 < xs !! 0))

main = do
  contents <- getContents
  let pairList = [splitNumbers line [] | line <- lines contents]
  let containedPairs = foldr (\x acc -> if isOverlap x then acc + 1 else acc) 0 pairList
  print containedPairs
