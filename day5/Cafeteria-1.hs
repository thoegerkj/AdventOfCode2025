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

countIDs :: [String] -> [String] -> Int
countIDs ids ranges = sum $ map (rangesContain ranges . read) ids

rangesContain :: [String] -> Int -> Int
rangesContain [] id = 0
rangesContain (r : rest) id
  | rangeContain r id = 1
  | otherwise = rangesContain rest id

rangeContain :: String -> Int -> Bool
rangeContain r n =
  let (n1, n2) = splitAtDash r
      aboveLower = n1 <= n
      belowHigher = n <= n2
   in aboveLower && belowHigher

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let (ranges, ids) = splitAtEmptyLine $ lines contents
      freshIdC = countIDs ids ranges
  print freshIdC
