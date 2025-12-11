readCoords :: String -> [(Int, Int)]
readCoords = map (\line -> let [x, y] = split ',' line in (read x :: Int, read y :: Int)) . lines
  where
    split :: Char -> String -> [String]
    split c = splitOnC []
      where
        splitOnC acc [] = [reverse acc]
        splitOnC acc (x : xs)
          | x == c = reverse acc : splitOnC [] xs
          | otherwise = splitOnC (x : acc) xs

findEdgeW :: [((Int, Int), Int)] -> [(Int, Int, Int)]
findEdgeW [] = []
findEdgeW (x : xs) = map (distance x) xs ++ findEdgeW xs
  where
    distance :: ((Int, Int), Int) -> ((Int, Int), Int) -> (Int, Int, Int)
    distance ((x1, y1), i1) ((x2, y2), i2) = ((abs (x1 - x2) + 1) * (abs (y1 - y2) + 1), i1, i2)

findLargestEdge :: [(Int, Int, Int)] -> (Int, Int, Int)
findLargestEdge = foldl1 maxEdge
  where
    maxEdge e1@(w1, _, _) e2@(w2, _, _)
      | w1 < w2 = e2
      | otherwise = e1

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let coords = readCoords contents
      coordsAndId = zip coords [0 ..]
      edges = findEdgeW coordsAndId
      smallestEdge = findLargestEdge edges
  print smallestEdge
