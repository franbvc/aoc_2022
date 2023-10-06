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

isLeftContained :: [Int] -> Bool
isLeftContained xs = (xs !! 0 >= xs !! 2) && (xs !! 1 <= xs !! 3)

isRightContained :: [Int] -> Bool
isRightContained xs = (xs !! 0 <= xs !! 2) && (xs !! 1 >= xs !! 3)

isContained :: [Int] -> Bool
isContained xs = isLeftContained xs || isRightContained xs

main = do
  contents <- getContents
  let pairList = [splitNumbers line [] | line <- lines contents]
  let containedPairs = foldr (\x acc -> if isContained x then acc + 1 else acc) 0 pairList
  print containedPairs
