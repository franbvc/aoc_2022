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

solve :: [(String, Int)] -> [Int] -> [Int]
solve instrList regValues
  | length instrList == 1 = regValues
  | otherwise = solve (tail instrList) (executeInstr (instrList !! 0) (instrList !! 1) regValues)

main = do
  contents <- readFile "adventofcode.com_2022_day_10_input.txt"
  -- contents <- readFile "test.txt"
  let instructions = map parseInstruction (lines contents)

  let regValues = solve instructions [1]

  print $ length regValues

  let signal20 = (regValues !! 19) * 20
  let signal60 = (regValues !! 59) * 60
  let signal100 = (regValues !! 99) * 100
  let signal140 = (regValues !! 139) * 140
  let signal180 = (regValues !! 179) * 180
  let signal220 = (regValues !! 219) * 220

  print $ signal20 + signal60 + signal100 + signal140 + signal180 + signal220