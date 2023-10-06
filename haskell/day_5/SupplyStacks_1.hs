import Data.Char
import Data.List as L

convertToList :: String -> [Char] -> [Char]
convertToList str acc
  | null str = acc
  | take 4 str == "    " = convertToList (drop 4 str) (acc ++ [' '])
  | otherwise = convertToList (drop 4 str) (acc ++ [str !! 1])

removeEmpty :: [[Char]] -> [[Char]]
removeEmpty = map (filter (/= ' '))

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

pop :: [a] -> (a, [a])
pop [] = (undefined, [])
pop (x : xs) = (x, xs)

push :: a -> [a] -> [a]
push x xs = x : xs

stackMove :: Int -> [Char] -> [Char] -> ([Char], [Char])
stackMove n from to
  | n == 0 = (from, to)
  | otherwise = stackMove (n - 1) updatedFrom (push toPop to)
  where
    (toPop, updatedFrom) = pop from

updateStackList :: Int -> [Char] -> [[Char]] -> [[Char]]
updateStackList n stack stackList = lhs ++ [stack] ++ rhs
  where
    (lhs, _ : rhs) = splitAt n stackList

runMoves :: [[Int]] -> [[Char]] -> [[Char]]
runMoves moveList stackList
  | null moveList = stackList
  | otherwise = runMoves (tail moveList) updatedStackList
  where
    [n, from, to] = take 3 moveList !! 0
    (newFrom, newTo) = stackMove n (stackList !! (from - 1)) (stackList !! (to - 1))
    updatedStackList = updateStackList (to - 1) newTo (updateStackList (from - 1) newFrom stackList)

main = do
  contents <- getContents
  let initialSupplyStacks = take 8 (lines contents)
  let moveList = drop 10 (lines contents)
  moveList <- return [splitNumbers move [] | move <- moveList]

  let supplyStackList = removeEmpty (transpose [convertToList x [] | x <- initialSupplyStacks])

  print $ runMoves moveList supplyStackList
