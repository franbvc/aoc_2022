import Data.List (sort)

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where
      (w, s'') = break p s'

data Monkey = Monkey
  { items :: [Int],
    op :: Int -> Int,
    testValue :: Int,
    true :: Int,
    false :: Int
  }

instance Show Monkey where
  show (Monkey items op testValue true false) =
    "Monkey "
      ++ show items
      ++ " "
      ++ show testValue
      ++ " "
      ++ show true
      ++ " "
      ++ show false

replaceItems :: Monkey -> [Int] -> Monkey
replaceItems monkey newItems = monkey {items = newItems}

addItems :: Monkey -> [Int] -> Monkey
addItems monkey newItems = monkey {items = items monkey ++ newItems}

removeFirstItem :: Monkey -> Monkey
removeFirstItem monkey = monkey {items = tail (items monkey)}

splitMonkeys :: [String] -> [[String]] -> [[String]]
splitMonkeys [] acc = acc
splitMonkeys input acc = splitMonkeys (drop 7 input) (acc ++ [take 6 input])

spaceOrComma :: Char -> Bool
spaceOrComma c = c == ' ' || c == ','

getMonkeyItems :: String -> [Int]
getMonkeyItems = map (read :: String -> Int) . drop 2 . wordsWhen (spaceOrComma)

lastItems :: [a] -> Int -> [a] -> [a]
lastItems [] _ acc = acc
lastItems _ 0 acc = acc
lastItems x n acc = lastItems (init x) (n - 1) ((last x) : acc)

parseOp :: String -> String -> String -> Int -> Int
parseOp lhs op rhs = case op of
  "+" -> if lhs /= rhs then (+) x else (* 2)
  "-" -> if lhs /= rhs then (-) x else (div 2)
  "*" -> if lhs /= rhs then (*) x else (^ 2)
  "/" -> if lhs /= rhs then div x else (flip div 2)
  _ -> error "Invalid operator"
  where
    x = read rhs :: Int

getMonkeyOp :: String -> Int -> Int
getMonkeyOp str = parseOp lhs op rhs
  where
    [lhs, op, rhs] = lastItems (words str) 3 []

getMonkeyTestValue :: String -> Int
getMonkeyTestValue = (read :: String -> Int) . last . words

getMonkeyThrow :: String -> Int
getMonkeyThrow = (read :: String -> Int) . last . words

parseMonkey :: [String] -> Monkey
parseMonkey input = Monkey items op testValue true false
  where
    items = getMonkeyItems (input !! 1)
    op = getMonkeyOp (input !! 2)
    testValue = getMonkeyTestValue (input !! 3)
    true = getMonkeyThrow (input !! 4)
    false = getMonkeyThrow (input !! 5)

worryLevel :: Int -> (Int -> Int) -> Int
worryLevel value op = floor (baseVal / 3)
  where
    baseVal :: Double = fromIntegral (op value)

isTestTrue :: Int -> Int -> Bool
isTestTrue testValue worryLevel = (worryLevel `mod` testValue) == 0

throwDestination :: Monkey -> (Int, Int)
throwDestination Monkey {items, op, testValue, true, false} = (val, dest)
  where
    val = worryLevel (head items) op
    dest = if testResult then true else false
    testResult = isTestTrue testValue val

replaceMonkey :: [Monkey] -> Int -> Monkey -> [Monkey]
replaceMonkey monkeyList index newMonkey =
  take index monkeyList ++ [newMonkey] ++ drop (index + 1) monkeyList

incrementListItem :: [Int] -> Int -> [Int]
incrementListItem list index = take index list ++ [newVal] ++ drop (index + 1) list
  where
    newVal = (list !! index) + 1

processThrow :: [Monkey] -> Int -> [Int] -> ([Monkey], [Int])
processThrow monkeyList index itemCount
  | index == length monkeyList = (monkeyList, itemCount)
  | items currMonkey == [] = processThrow monkeyList (index + 1) itemCount
  | otherwise = processThrow newMonkeyList index newItemCount
  where
    currMonkey = monkeyList !! index
    (val, dest) = throwDestination currMonkey
    updatedCurrMonkey = removeFirstItem currMonkey
    updatedReceivingMonkey = addItems (monkeyList !! dest) [val]
    newMonkeyList = replaceMonkey (replaceMonkey monkeyList index updatedCurrMonkey) dest updatedReceivingMonkey
    newItemCount = incrementListItem itemCount index

getItemCount :: [Monkey] -> [Int]
getItemCount = map (length . items)

iterateN :: Int -> [Monkey] -> [Int] -> [Int]
iterateN iter monkeyList itemCount
  | iter == 0 = itemCount
  | otherwise = iterateN (iter - 1) newMonkeyList newItemCount
  where
    (newMonkeyList, newItemCount) = processThrow monkeyList 0 itemCount

main = do
  contents <- readFile "adventofcode.com_2022_day_11_input.txt"
  -- contents <- readFile "test.txt"

  let monkeyList = map parseMonkey (splitMonkeys (lines contents) [])
  let initialItemCount = getItemCount monkeyList

  let finalItemCount = iterateN 20 monkeyList [0, 0, 0, 0, 0, 0, 0, 0]
  let twoLargest = (take 2 . reverse . sort) finalItemCount

  print $ product twoLargest
