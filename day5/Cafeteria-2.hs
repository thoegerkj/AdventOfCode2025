splitAtEmptyLine :: [String] -> ([String], [String])
splitAtEmptyLine s = sael s []
  where
    sael :: [String] -> [String] -> ([String], [String])
    -- sael [] acc = (reverse acc, [])
    sael ("" : t) acc = (reverse acc, t)
    sael (h : t) acc = sael t (h : acc)

splitAtDash :: String -> (Int, Int)
splitAtDash s = go s []
  where
    -- go [] acc = (
    go ('-' : t) acc = (read (reverse acc), read t)
    go (h : t) acc = go t (h : acc)

sort :: [(Int, Int)] -> [(Int, Int)]
sort [] = []
sort [x] = [x]
sort (x : xs) =
  let (p1, p2) = partition xs x [] []
      sp1 = sort p1
      sp2 = sort p2
   in sp1 ++ (x : sp2)
  where
    partition :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)] -> ([(Int, Int)], [(Int, Int)])
    partition [] x acc1 acc2 = (acc1, acc2)
    partition (x1 : xs) x acc1 acc2
      | fst x < fst x1 = partition xs x acc1 (x1 : acc2)
      | otherwise = partition xs x (x1 : acc1) acc2

merge :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
merge acc [] = [acc]
merge (ns, ne) ((x1, y1) : xs)
  | x1 > ne = (ns, ne) : merge (x1, y1) xs
  -- In the below cases, x1 <= ne so the ranges are in continuation
  | y1 <= ne = merge (ns, ne) xs
  | y1 > ne = merge (ns, y1) xs

-- Should be exhaustive

count :: [(Int, Int)] -> Int
count =
  foldl
    (\res (a, b) -> res + (b - a + 1))
    0

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let (r, _) = splitAtEmptyLine $ lines contents
      ranges = map splitAtDash r
      sortedRanges = sort ranges
      cleanRanges = merge (head sortedRanges) (tail sortedRanges)
      countIDs = count cleanRanges
  -- print sortedRanges
  -- print cleanRanges -- countIDs
  print countIDs
