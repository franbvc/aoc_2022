import Data.Set as S

verifySequence :: [Char] -> Bool
verifySequence xs = S.size (S.fromList xs) == length xs

findSequence :: [Char] -> Int -> Int
findSequence xs acc
  | verifySequence (Prelude.take 14 xs) = acc
  | otherwise = findSequence (Prelude.drop 1 xs) (acc + 1)

main = do
  contents <- getContents

  print $ findSequence contents 14