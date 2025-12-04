solve :: [String] -> Int
solve inputLines =
  let height = length inputLines
      width = length (head inputLines)
      grid = pad inputLines
      onePass cres cgrid =
        foldl
          (\(res, ngrid) i -> countAndRemoveRow cgrid i res ngrid)
          (cres, cgrid)
          [1 .. height]
      removeAndTryAgain cres cgrid =
        let (nres, ngrid) = onePass cres cgrid
         in if cres == nres
              then nres
              else removeAndTryAgain nres ngrid
   in removeAndTryAgain 0 grid

countAndRemoveRow :: [String] -> Int -> Int -> [String] -> (Int, [String])
countAndRemoveRow grid row cres ngrid =
  let width = length (head grid) - 2
   in foldl
        (\(res, ngrid) j -> countAndRemove grid row j res ngrid)
        (cres, ngrid)
        [1 .. width]

countAndRemove :: [String] -> Int -> Int -> Int -> [String] -> (Int, [String])
countAndRemove grid row col cres ngrid
  | grid !! row !! col == '.' = (cres, ngrid)
  | tooManyNeighbors grid row col = (cres, ngrid) -- ???
  | otherwise = (cres + 1, updateGrid ngrid row col)
  where
    updateGrid ngrid row col =
      let beforeRow = take row ngrid
          afterRow = drop (row + 1) ngrid
          currentRow = ngrid !! row
          newRow = take col currentRow ++ "." ++ drop (col + 1) currentRow
       in beforeRow ++ [newRow] ++ afterRow

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
