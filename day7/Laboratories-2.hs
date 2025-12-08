import Data.Map (Map, empty, insert, member, (!))

find :: Char -> String -> Int
find c (x : xs)
  | c == x = 0
  | otherwise = 1 + find c xs

step :: [String] -> Int -> Int -> Map (Int, Int) Int -> Map (Int, Int) Int
step grid row i memo
  | member (row, i) memo = memo
  | row == length grid = insert (row, i) 1 memo
  | otherwise =
      let char = grid !! row !! i
       in case char of
            '.' ->
              let memo' = step grid (row + 1) i memo
               in insert (row, i) (memo' ! (row + 1, i)) memo'
            '^' ->
              let memo1 = step grid (row + 1) (i - 1) memo
                  memo2 = step grid (row + 1) (i + 1) memo1
               in insert (row, i) (memo2 ! (row + 1, i - 1) + memo2 ! (row + 1, i + 1)) memo2

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let grid = lines contents
      start = find 'S' (head grid)
      result = step grid 1 start empty
  print $ result ! (1, start)
