import Data.Char (isDigit)

data PacketItem = PacketInt Int | PacketList [PacketItem] deriving (Show, Read, Eq, Ord)

parseLine :: String -> [Either Char Int]
parseLine = go
  where
    go [] = []
    go (c : cs)
      | c == '[' || c == ']' = Left c : go cs
      | isDigit c =
          let (digits, rest) = span isDigit (c : cs)
           in Right (read digits) : go rest
      | otherwise = go cs

takeWhileOpenList :: [Either Char Int] -> [Either Char Int]
takeWhileOpenList strList = go strList 1
  where
    go [] _ = []
    go (x : xs) lParenCount
      | lParenCount == 0 = []
      | x == Left '[' = x : go xs (lParenCount + 1)
      | x == Left ']' && lParenCount > 0 = x : go xs (lParenCount - 1)
      | otherwise = x : go xs lParenCount

listToPacketItems :: [Either Char Int] -> [PacketItem]
listToPacketItems [] = []
listToPacketItems (Right x : xs) = PacketInt x : listToPacketItems xs
listToPacketItems (Left '[' : xs) =
  PacketList (listToPacketItems newList) : listToPacketItems (drop (length newList) xs)
  where
    newList = takeWhileOpenList xs
listToPacketItems (Left ']' : xs) = listToPacketItems xs

compareInt :: PacketItem -> PacketItem -> Maybe Bool
compareInt (PacketInt x) (PacketInt y)
  | x < y = Just True
  | x > y = Just False
  | otherwise = Nothing
compareInt _ _ = error "Invalid comparison"

compareList :: PacketItem -> PacketItem -> Maybe Bool
compareList (PacketList []) (PacketList []) = Nothing
compareList (PacketList []) (PacketList (x : xs)) = Just True
compareList (PacketList (x : xs)) (PacketList []) = Just False
compareList (PacketList (x : xs)) (PacketList (y : ys)) = case compareItems x y of
  Just True -> Just True
  Just False -> Just False
  Nothing -> compareList (PacketList xs) (PacketList ys)

compareItems :: PacketItem -> PacketItem -> Maybe Bool
compareItems (PacketInt x) (PacketInt y) = compareInt (PacketInt x) (PacketInt y)
compareItems (PacketList x) (PacketList y) = compareList (PacketList x) (PacketList y)
compareItems (PacketInt x) (PacketList y) = compareItems (PacketList [PacketInt x]) (PacketList y)
compareItems (PacketList x) (PacketInt y) = compareItems (PacketList x) (PacketList [PacketInt y])

getPacketPairs :: [[Either Char Int]] -> [(PacketItem, PacketItem)]
getPacketPairs [] = []
getPacketPairs (x : y : _ : xs) = (head (listToPacketItems x), head (listToPacketItems y)) : getPacketPairs xs
getPacketPairs (x : y : xs) = (head (listToPacketItems x), head (listToPacketItems y)) : getPacketPairs xs

main = do
  -- contents <- readFile "test.txt"
  contents <- readFile "adventofcode.com_2022_day_13_input.txt"

  let input = lines contents
  let tokens = map parseLine input
  let pairs = getPacketPairs tokens

  let results = map (uncurry compareItems) pairs

  print $ sum [index + 1 | (index, result) <- zip [0 ..] results, result == Just True]
