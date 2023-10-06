import Data.List
import Data.Maybe

choiceList_A = ['A', 'B', 'C']

convertMove :: String -> (Int, Char)
convertMove str = do
  let opponent = head str `elemIndex` choiceList_A
  if isJust opponent
    then (fromJust opponent, last str)
    else error "Invalid choice"

playerMove :: (Int, Char) -> Int
playerMove pair 
  | snd pair == 'Y' = fst pair
  | snd pair == 'X' = (fst pair + 2) `mod` 3
  | snd pair == 'Z' = (fst pair + 1) `mod` 3
  | otherwise = error "Invalid choice"

roundScore :: (Int, Char) -> Int
roundScore pair 
  | snd pair == 'X' = 0 + playerMove pair + 1
  | snd pair == 'Y' = 3 + playerMove pair + 1
  | snd pair == 'Z' = 6 + playerMove pair + 1
  | otherwise = error "Invalid choice"


main = do
  contents <- getContents
  print $ sum . map (roundScore . convertMove) $ lines contents