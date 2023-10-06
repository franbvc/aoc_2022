import Control.Monad.Cont (cont)
import Data.List (sort)

replaceEmpty :: String -> String
replaceEmpty s = if s == "" then "-1" else s

getSumList :: [Int] -> [Int] -> Int -> [Int]
getSumList xs ys acc
  | null xs = ys
  | head xs == -1 = getSumList (tail xs) (ys++[acc]) 0
  | otherwise = getSumList (tail xs) ys (acc + head xs)


main = do
  contents <- getContents
  let xs = [read (replaceEmpty x) :: Int | x <- lines contents]
  let sumList = getSumList xs [] 0
  let sortedSumList = reverse . sort $ sumList
  print $ foldr(+) 0 (take 3 sortedSumList)