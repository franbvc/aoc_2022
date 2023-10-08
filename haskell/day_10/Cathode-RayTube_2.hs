import Data.Text.Internal.Lazy (chunk)

parseInstruction :: String -> (String, Int)
parseInstruction str
  | str == "noop" = ("noop", 0)
  | otherwise = ("addx", rhs)
  where
    parts = words str
    lhs = head parts
    rhs = read (last parts) :: Int

executeInstr :: (String, Int) -> (String, Int) -> [Int] -> [Int]
executeInstr (currInstr, currInstrValue) (nextInstr, nextInstrValue) regValues
  | currInstr == "noop" && nextInstr == "noop" = regValues ++ [last regValues]
  | currInstr == "noop" && nextInstr == "addx" = regValues ++ [last regValues] ++ [last regValues]
  | currInstr == "addx" && nextInstr == "addx" = regValues ++ [(last regValues) + currInstrValue] ++ [(last regValues) + currInstrValue]
  | otherwise = regValues ++ [(last regValues) + currInstrValue]

splitIntoChunks :: Int -> [a] -> [[a]]
splitIntoChunks _ [] = []
splitIntoChunks n xs = (take n xs) : (splitIntoChunks n (drop n xs))

solve :: [(String, Int)] -> [Int] -> [Int]
solve instrList regValues
  | length instrList == 1 = regValues
  | otherwise = solve (tail instrList) (executeInstr (instrList !! 0) (instrList !! 1) regValues)

printCRTLine :: [Int] -> IO ()
printCRTLine line = do
  let lineStr = map (\x -> if x == 0 then '.' else '#') line
  putStrLn lineStr

printCRT :: [[Int]] -> IO ()
printCRT = mapM_ printCRTLine

isSpriteOnClock :: Int -> Int -> Bool
isSpriteOnClock clock sprite = abs (clock - sprite) <= 1

fillCRT :: [Int] -> [Int] -> [Int]
fillCRT [] crtOutput = crtOutput
fillCRT (x : xs) crtOutput = fillCRT xs newCrtOutput
  where
    (clock, sprite) = (length crtOutput, x)
    newCrtOutput = if isSpriteOnClock clock sprite then crtOutput ++ [1] else crtOutput ++ [0]

main = do
  contents <- readFile "adventofcode.com_2022_day_10_input.txt"
  -- contents <- readFile "test.txt"
  let instructions = map parseInstruction (lines contents)

  let regValues = solve instructions [1]

  let crtLines = map (`fillCRT` []) (splitIntoChunks 40 regValues)

  printCRT crtLines
