import Data.Set as S

data Point = Point Int Int deriving (Show, Eq, Ord)

pointsDistance :: Point -> Point -> Int
pointsDistance (Point x1 y1) (Point x2 y2) = abs (x1 - x2) + abs (y1 - y2)

parseLine :: String -> (Char, Int)
parseLine line = (lhs, rhs)
  where
    parts = words line
    lhs = head $ head parts
    rhs = read (last parts) :: Int

moveHead :: Point -> Char -> Point
moveHead (Point x y) direction = case direction of
  'U' -> Point x (y + 1)
  'D' -> Point x (y - 1)
  'R' -> Point (x + 1) y
  'L' -> Point (x - 1) y

moveTail :: Point -> Point -> Point
moveTail (Point tailX tailY) (Point headX headY)
  | dist <= 1 = Point tailX tailY
  | (not sameX) && (not sameY) && dist <= 2 = Point tailX tailY
  | sameX = Point tailX newTailY
  | sameY = Point newTailX tailY
  | otherwise = Point newTailX newTailY
  where
    dist = pointsDistance (Point tailX tailY) (Point headX headY)
    sameX = tailX == headX
    sameY = tailY == headY
    newTailX = if tailX < headX then tailX + 1 else tailX - 1
    newTailY = if tailY < headY then tailY + 1 else tailY - 1

makeMove :: (Char, Int) -> Set Point -> (Point, Point) -> (Set Point, (Point, Point))
makeMove (direction, distance) tailVisited (head, tail)
  | distance == 0 = (tailVisited, (head, tail))
  | otherwise = makeMove (direction, distance - 1) newVisited (newHead, newTail)
  where
    newHead = moveHead head direction
    newTail = moveTail tail newHead
    newVisited = S.union tailVisited (S.fromList [newTail])

solve :: [(Char, Int)] -> Set Point -> (Point, Point) -> Set Point
solve moveList visited (head, tail)
  | Prelude.null moveList = visited
  | otherwise = solve (Prelude.drop 1 moveList) newVisited (newHead, newTail)
  where
    move = moveList !! 0
    (newVisited, (newHead, newTail)) = makeMove move visited (head, tail)

main = do
  contents <- getContents
  let moveList = Prelude.map parseLine (lines contents)
  let visited = solve moveList S.empty (Point 0 0, Point 0 0)

  print $ length visited
