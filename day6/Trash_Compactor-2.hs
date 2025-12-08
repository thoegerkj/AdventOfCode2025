getColumns :: [String] -> [(String, Char)]
getColumns s
  | all null s = []
  | otherwise =
      ( map head (take (length s - 1) s),
        head $ last s
      )
        : getColumns (map (drop 1) s)

stripColumns :: [(String, Char)] -> [(String, Char)]
stripColumns [] = []
stripColumns ((s, c) : scs) = (strip s, c) : stripColumns scs
  where
    strip :: String -> String
    strip =
      reverse
        . foldl
          ( \acc s1 ->
              if s1 == ' '
                then acc
                else s1 : acc
          )
          []

cleanColumns :: [(String, Char)] -> [(Int, Char)]
cleanColumns [] = []
cleanColumns (("", _) : ss) = cleanColumns ss
cleanColumns ((s, c) : ss) = (read s, c) : cleanColumns ss

solveAndAdd :: [Int] -> [(Int, Char)] -> Int
solveAndAdd _ [] = 0
solveAndAdd acc ((n, ' ') : t) = solveAndAdd (n : acc) t
solveAndAdd acc ((n, '*') : t) = product (n : acc) + solveAndAdd [] t
solveAndAdd acc ((n, '+') : t) = sum (n : acc) + solveAndAdd [] t

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let columns = getColumns $ map reverse $ lines contents
      strippedColumns = stripColumns columns
      cleanedColumns = cleanColumns strippedColumns
      solution = solveAndAdd [] cleanedColumns
  -- print columns
  -- print strippedColumns
  -- print cleanedColumns
  print solution
