import Data.Char (digitToInt)
import Data.List (transpose)

leftCount :: [Int] -> Int -> Int -> Int
leftCount row currHeight acc
  | null row = acc
  | head row >= currHeight = acc + 1
  | otherwise = leftCount (tail row) currHeight (acc + 1)

horizontalScore :: [Int] -> Int -> Int
horizontalScore row pos = (leftCount (reverse left) mid 0) * (leftCount right mid 0)
  where
    mid = row !! pos
    left = take pos row
    right = drop (pos + 1) row

scenicScoreH :: [[Int]] -> [[Int]] -> [[Int]]
scenicScoreH mat outMat
  | null mat = outMat
  | otherwise = scenicScoreH (tail mat) (outMat ++ [map (horizontalScore (head mat)) [0 .. (length (head mat) - 1)]])

scenicScoreV :: [[Int]] -> [[Int]] -> [[Int]]
scenicScoreV mat outMat = transpose $ scenicScoreH (transpose mat) (transpose outMat)

printMatrix :: [[Int]] -> IO ()
printMatrix = mapM_ print

mergeRow :: [Int] -> [Int] -> [Int]
mergeRow = zipWith (*)

mergeMatrix :: [[Int]] -> [[Int]] -> [[Int]]
mergeMatrix mat1 mat2 = [mergeRow x y | (x, y) <- zip mat1 mat2]

main :: IO ()
main = do
  contents <- getContents
  let mat = [map digitToInt line | line <- lines contents]
  let scoreH = scenicScoreH mat []
  let scoreV = scenicScoreV mat []
  let totalScore = mergeMatrix scoreH scoreV

  print $ maximum (concat totalScore)
