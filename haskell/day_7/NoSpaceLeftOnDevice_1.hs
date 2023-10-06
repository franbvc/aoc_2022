import Data.Char as C
import Data.Map as M
import Data.Maybe as MB
import Text.Parsec (parse)

data Item
  = Folder
      { name :: String
      }
  | File
      { name :: String,
        size :: Int
      }
  deriving (Show)

-- List --

parseFile :: String -> Item
parseFile line = File name (read size :: Int)
  where
    size = takeWhile (/= ' ') line
    name = Prelude.drop (1 + length size) line

parseFolder :: String -> Item
parseFolder line = Folder name
  where
    name = Prelude.drop 1 (dropWhile (/= ' ') line)

parseItem :: String -> Item
parseItem line
  | C.isNumber firstChar = parseFile line
  | otherwise = parseFolder line
  where
    firstChar = head line

addItemToMap :: String -> Item -> Map String [Item] -> Map String [Item]
addItemToMap path item map = M.insert path (item : (MB.fromMaybe [] $ M.lookup path map)) map

-- Path --

pathRemoveLevel :: String -> String
pathRemoveLevel path = reverse $ Prelude.drop 1 $ Prelude.dropWhile (/= '/') $ reverse path

pathAddLevel :: String -> String -> String
pathAddLevel path level = path ++ "/" ++ level

updatePath :: String -> String
updatePath xs = execCommand lhs rhs
  where
    (lhs, rhs) = parseCommand xs

-- Command --

isCommand :: String -> Bool
isCommand = (== '$') . head

parseCommand :: String -> (String, String)
parseCommand str = if length cmdList == 1 then (head cmdList, "") else (head cmdList, last cmdList)
  where
    cmdList = tail $ words str

execCommand :: String -> String -> String
execCommand line currFolder
  | cmd == ("cd", "..") = pathRemoveLevel currFolder
  | fst cmd == "cd" = pathAddLevel currFolder (snd cmd)
  | otherwise = currFolder
  where
    cmd = parseCommand line

parseLines :: [String] -> String -> Map String [Item] -> Map String [Item]
parseLines lines currFolder currMap
  | Prelude.null lines = currMap
  | isCommand $ head lines = parseLines (tail lines) (execCommand (head lines) currFolder) currMap
  | otherwise = parseLines (tail lines) currFolder (addItemToMap currFolder (parseItem (head lines)) currMap)

findItems :: String -> Map String [Item] -> [Item]
findItems path currMap = case M.lookup path currMap of
  Just items -> items
  Nothing -> error "Item not found"

getItemSize :: Item -> String -> Map String [Item] -> Int
getItemSize item parentFolder currMap = case item of
  File _ size -> size
  Folder name -> getFolderSize (pathAddLevel parentFolder name) (findItems (pathAddLevel parentFolder name) currMap) currMap 0

getFolderSize :: String -> [Item] -> Map String [Item] -> Int -> Int
getFolderSize folderName folderItems currMap acc
  | Prelude.null folderItems = acc
  | otherwise = getFolderSize folderName (tail folderItems) currMap (acc + getItemSize (head folderItems) folderName currMap)

getNewMap :: Map String [Item] -> Map String Int
getNewMap currMap = M.mapWithKey (\k a -> getFolderSize k a currMap 0) currMap

sumIf :: Map String Int -> Int -> Int
sumIf currMap maxSize = sum . Prelude.filter (<= maxSize) $ M.elems currMap

main = do
  contents <- getContents
  let lines = Prelude.lines contents
  lines <- return $ Prelude.drop 1 lines

  print $ sumIf (getNewMap $ parseLines lines "" M.empty) 100000
