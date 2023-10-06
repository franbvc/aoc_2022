import Control.Monad.Cont (cont)

replaceEmpty :: String -> String
replaceEmpty s = if s == "" then "-1" else s

getNextSum :: [Int] -> [Int] -> Int -> [Int]
getNextSum xs ys acc
  | null xs = ys
  | head xs == -1 = getNextSum (tail xs) (ys ++ [acc]) 0
  | otherwise = getNextSum (tail xs) ys (acc + head xs)

main = do
  contents <- getContents
  let xs = [read (replaceEmpty x) :: Int | x <- lines contents]
  -- let xs = map (read . replaceEmpty) $ lines contents :: [Int]
  let foo = getNextSum xs [] 0
  print $ foldr (max) 0 foo