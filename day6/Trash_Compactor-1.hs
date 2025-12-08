split :: Char -> String -> [String]
split c = splitOnC []
  where
    splitOnC acc [] = [reverse acc]
    splitOnC acc (x : xs)
      | x == c = reverse acc : splitOnC [] (removeLeadingSpace xs)
      | otherwise = splitOnC (x : acc) xs

removeLeadingSpace :: String -> String
removeLeadingSpace (' ' : xs) = removeLeadingSpace xs
removeLeadingSpace s = s

removeIfEmpty :: [String] -> [String]
removeIfEmpty =
  reverse
    . foldl
      ( \acc s ->
          if null s
            then acc
            else s : acc
      )
      []

zipE :: [[String]] -> [([Int], Char)]
zipE s
  | all null s = []
  | otherwise = (map (read . head) (take n s), promiseChar $ head $ last s) : zipE (map (drop 1) s)
  where
    n = length s - 1
    promiseChar :: String -> Char
    promiseChar [c] = c

solveAndAdd :: [([Int], Char)] -> Int
solveAndAdd eqs = sum $ map solve eqs

solve :: ([Int], Char) -> Int
solve (nums, '*') = product nums
solve (nums, '+') = sum nums

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let rows = lines contents
      rowsSplitClean = map (removeIfEmpty . split ' ' . removeLeadingSpace) rows
      equations = zipE rowsSplitClean
      solution = solveAndAdd equations
  print rows
  print rowsSplitClean
  print solution
