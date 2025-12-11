import Data.HashSet qualified as HashSet

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

findEdgeW :: [((Int, Int), Int)] -> HashSet.HashSet (Int, Int) -> [(Int, Int, Int)]
findEdgeW [] _ = []
findEdgeW (x : xs) set = map (distance set x) xs ++ findEdgeW xs set
  where
    distance :: HashSet.HashSet (Int, Int) -> ((Int, Int), Int) -> ((Int, Int), Int) -> (Int, Int, Int)
    distance bound ((x1, y1), i1) ((x2, y2), i2) =
      let size = ((abs (x1 - x2) + 1) * (abs (y1 - y2) + 1), i1, i2)
          boundaryPoints = [(x1, y) | y <- [min y1 y2 .. max y1 y2]] ++ [(x2, y) | y <- [min y1 y2 .. max y1 y2]] ++ [(x, y1) | x <- [min x1 x2 .. max x1 x2]] ++ [(x, y2) | x <- [min x1 x2 .. max x1 x2]]
          inside = all (\p -> not $ HashSet.member p bound) boundaryPoints
       in if inside then size else (0, i1, i2)

findLargestEdge :: [(Int, Int, Int)] -> (Int, Int, Int)
findLargestEdge = foldl1 maxEdge
  where
    maxEdge e1@(w1, _, _) e2@(w2, _, _)
      | w1 < w2 = e2
      | otherwise = e1

buildJustOutsideSet :: [(Int, Int)] -> HashSet.HashSet (Int, Int)
buildJustOutsideSet coords = trim (coords ++ [head coords]) $ buildSet (coords ++ [head coords]) HashSet.empty
  where
    buildSet :: [(Int, Int)] -> HashSet.HashSet (Int, Int) -> HashSet.HashSet (Int, Int)
    buildSet [_] acc = acc
    buildSet ((x1, y1) : xs@((x2, y2) : _)) acc
      | x1 < x2 = buildSet xs (foldr HashSet.insert acc [(x', y1 - 1) | x' <- [x1 .. x2]])
      | x1 > x2 = buildSet xs (foldr HashSet.insert acc [(x', y1 + 1) | x' <- [x2 .. x1]])
      | y1 < y2 = buildSet xs (foldr HashSet.insert acc [(x1 + 1, y') | y' <- [y1 .. y2]])
      | y1 > y2 = buildSet xs (foldr HashSet.insert acc [(x1 - 1, y') | y' <- [y2 .. y1]])

    trim :: [(Int, Int)] -> HashSet.HashSet (Int, Int) -> HashSet.HashSet (Int, Int)
    trim [_] acc = acc
    trim ((x1, y1) : xs@((x2, y2) : _)) acc
      | x1 < x2 = trim xs (foldr HashSet.delete acc [(x', y1) | x' <- [x1 .. x2]])
      | x1 > x2 = trim xs (foldr HashSet.delete acc [(x', y1) | x' <- [x2 .. x1]])
      | y1 < y2 = trim xs (foldr HashSet.delete acc [(x1, y') | y' <- [y1 .. y2]])
      | y1 > y2 = trim xs (foldr HashSet.delete acc [(x1, y') | y' <- [y2 .. y1]])

main :: IO ()
main = do
  contents <- readFile "example.txt"
  let coords = readCoords contents
      justOutSide = buildJustOutsideSet coords
      coordsAndId = zip coords [0 ..]
      edges = findEdgeW coordsAndId justOutSide
      (smallestEdge, p1, p2) = findLargestEdge edges
  -- print justOutSide
  print $ coords !! p1
  print $ coords !! p2
  print smallestEdge
