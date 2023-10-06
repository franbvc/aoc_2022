import Data.Char (digitToInt)
import Data.List (transpose)

dropEdges :: [[Int]] -> [[Int]]
dropEdges = init . tail

leftHighest :: [Int] -> [Int] -> [Int]
leftHighest inList outList
  | null inList = outList
  | null outList = leftHighest (tail inList) [head inList]
  | head inList > last outList = leftHighest (tail inList) (outList ++ [head inList])
  | otherwise = leftHighest (tail inList) (outList ++ [last outList])

visibleLeft :: [Int] -> Int -> [Int]
visibleLeft row maxVal
  | null row = []
  | head row > maxVal = -1 : visibleLeft (tail row) (head row)
  | otherwise = head row : visibleLeft (tail row) maxVal

visibleRow :: [Int] -> [Int]
visibleRow row = do
  let left = visibleLeft row 0
  let right = reverse $ visibleLeft (reverse row) 0
  zipWith (\x y -> if x == -1 || y == -1 then -1 else x) left right

visibleRows :: [[Int]] -> [[Int]] -> [[Int]]
visibleRows mat outMat
  | null mat = outMat
  | otherwise = visibleRows (tail mat) (outMat ++ [visibleRow (head mat)])

visible :: [[Int]] -> [[Int]]
visible mat = [head mat] ++ (visibleRows (dropEdges mat) []) ++ [last mat]

printMatrix :: [[Int]] -> IO ()
printMatrix = mapM_ print

mergeRow :: [Int] -> [Int] -> [Int]
mergeRow = zipWith (\x y -> if x == -1 || y == -1 then -1 else x)

mergeMatrix :: [[Int]] -> [[Int]] -> [[Int]]
mergeMatrix mat1 mat2 = [mergeRow x y | (x, y) <- zip mat1 mat2]

removeLayer :: [[Int]] -> [[Int]]
removeLayer = transpose . dropEdges . transpose . dropEdges

countEdges :: [[Int]] -> Int
countEdges mat = 4 * length (head mat) - 4

countVisible :: [[Int]] -> Int
countVisible mat = (countEdges mat) + length (filter (== -1) (concat (removeLayer mat)))

main = do
  contents <- getContents
  let mat = [map digitToInt line | line <- lines contents]

  -- print $ length mat
  -- print $ length (head mat)

  -- printMatrix mat
  -- printMatrix $ removeLayer mat

  let visibleHoriz = visible mat
  let visibleVert = transpose $ visible $ transpose mat

  print $ countVisible $ mergeMatrix visibleHoriz visibleVert
