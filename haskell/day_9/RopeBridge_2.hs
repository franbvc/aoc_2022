import Data.IntMap (update)
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

makeMove :: (Char, Int) -> Set Point -> [Point] -> (Set Point, [Point])
makeMove (direction, distance) tailVisited nodeList
  | distance == 0 = (tailVisited, nodeList)
  | otherwise = makeMove (direction, distance - 1) newVisited (newNodeList ++ [newTail])
  where
    newNodeList = updateNodes (tail nodeList) [moveHead (head nodeList) direction]
    newTail = moveTail (last nodeList) (last newNodeList)
    newVisited = S.union tailVisited (S.fromList [newTail])

-- list example: [Head, body, body, body, Tail] -> [body, body, body, tail] + [head] -> [body, body, tail] + [head, body]
updateNodes :: [Point] -> [Point] -> [Point]
updateNodes [] result = init result
updateNodes (x : xs) result = updateNodes xs (result ++ [moveTail x (last result)])

solve :: [(Char, Int)] -> Set Point -> [Point] -> Set Point
solve [] visited _ = visited
solve (move : moveList) visited nodeList = solve moveList newVisited newNodeList
  where
    (newVisited, newNodeList) = makeMove move visited nodeList

main = do
  contents <- getContents
  let moveList = Prelude.map parseLine (lines contents)
  let initialNodeList = [Point 0 0 | x <- [0 .. 9]]

  let visited = solve moveList S.empty initialNodeList

  print $ length visited
