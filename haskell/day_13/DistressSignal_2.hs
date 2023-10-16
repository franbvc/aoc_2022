import Data.Char (isDigit)
import Data.List (elemIndex, sort)
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust, isJust)
import Data.Set qualified as S

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

findAllPointers :: PacketItem -> [PacketItem] -> S.Set PacketItem
findAllPointers p pList = go p pList S.empty
  where
    go _ [] acc = acc
    go p (x : xs) acc
      | compareItems x p == Just True = go p xs (x `S.insert` acc)
      | otherwise = go p xs acc

pointedToPointers :: [PacketItem] -> M.Map PacketItem (S.Set PacketItem)
pointedToPointers xs = go M.empty xs xs
  where
    go acc [] _ = acc
    go acc (x : xs) pList = go (M.insert x (findAllPointers x pList) acc) xs pList

findNextPacket :: M.Map PacketItem (S.Set PacketItem) -> PacketItem
findNextPacket = go . M.toList
  where
    go [] = error "No next packet"
    go ((x, y) : xs)
      | null y = x
      | otherwise = go xs

removePacket :: PacketItem -> M.Map PacketItem (S.Set PacketItem) -> M.Map PacketItem (S.Set PacketItem)
removePacket p pMap = go (M.toList pMap) M.empty
  where
    go [] acc = acc
    go ((x, y) : xs) acc
      | x == p = go xs acc
      | otherwise = go xs (M.insert x (S.delete p y) acc)

organizePackets :: [PacketItem] -> [PacketItem]
organizePackets xs = do
  let pMap = pointedToPointers xs
  go pMap []
  where
    go pMap acc
      | null pMap = reverse acc
      | otherwise =
          let nextPacket = findNextPacket pMap
              newMap = removePacket nextPacket pMap
           in go newMap (nextPacket : acc)

main = do
  -- contents <- readFile "test.txt"
  contents <- readFile "adventofcode.com_2022_day_13_input.txt"

  let input = lines contents
  let tokens = Prelude.map parseLine input
  let pairs = getPacketPairs tokens
  let rightPairs = Prelude.filter (\(x, y) -> compareItems x y == Just True) pairs
  let initialMap = M.fromList rightPairs

  let divider1 = PacketList [PacketList [PacketInt 2]]
  let divider2 = PacketList [PacketList [PacketInt 6]]

  let allPackets = Prelude.map fst pairs ++ Prelude.map snd pairs ++ [divider1, divider2]

  let sortedPackets = organizePackets allPackets
  let divider1Index = (+ 1) <$> elemIndex divider1 sortedPackets
  let divider2Index = (+ 1) <$> elemIndex divider2 sortedPackets

  print $ (*) <$> divider1Index <*> divider2Index
