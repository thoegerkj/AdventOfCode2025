solve :: [String] -> Int
solve inputLines =
  let height = length inputLines
      width = length (head inputLines)
      grid = pad inputLines
   in foldl
        (\res i -> res + countRow grid i)
        0
        [1 .. height]

countRow :: [String] -> Int -> Int
countRow grid row =
  let width = length (head grid) - 2
   in foldl
        (\res j -> res + count grid row j)
        0
        [1 .. width]

count :: [String] -> Int -> Int -> Int
count grid row col
  | grid !! row !! col == '.' = 0
  | tooManyNeighbors grid row col = 0
  | otherwise = 1

-- Precondition: row and col are not on the border. we pad the grid to avoid this.
tooManyNeighbors :: [String] -> Int -> Int -> Bool
tooManyNeighbors grid row col = countNeighbors grid row col >= 4

countNeighbors :: [String] -> Int -> Int -> Int
countNeighbors grid row col =
  foldl
    ( \res dr ->
        foldl
          ( \res dc ->
              if dr == 0 && dc == 0
                then res
                else
                  if grid !! (row + dr) !! (col + dc) == '@'
                    then res + 1
                    else res
          )
          res
          [-1, 0, 1]
    )
    0
    [-1, 0, 1]

pad :: [String] -> [String]
pad lines =
  let height = length lines
      width = length (head lines)
      emptyRow = replicate (width + 2) '.'
      paddedLines = map (\line -> '.' : line ++ ".") lines
   in emptyRow : paddedLines ++ [emptyRow]

main :: IO ()
main = do
  contents <- readFile "input.txt"
  print $ solve $ lines contents
