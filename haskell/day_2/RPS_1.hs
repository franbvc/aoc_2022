import Data.List
import Data.Maybe

choiceList_A = ['A', 'B', 'C']

choiceList_B = ['X', 'Y', 'Z']

roundChoices :: String -> (Char, Char)
roundChoices str = (head str, last str)

convertChoices :: (Char, Char) -> (Int, Int)
convertChoices choices = do
  let opponent = fst choices `elemIndex` choiceList_A
  let player = snd choices `elemIndex` choiceList_B
  if isJust opponent && isJust player
    then (fromJust opponent, fromJust player)
    else error "Invalid choice"

roundWinner :: (Int, Int) -> Int
roundWinner choices
  | uncurry (==) choices = 3
  | fst choices == (snd choices + 2) `mod` 3 = 6
  | otherwise = 0

roundScore :: (Int, Int) -> Int
roundScore choices = roundWinner choices + snd choices + 1

main = do
  contents <- getContents
  print $ sum . map (roundScore . convertChoices . roundChoices) $ lines contents