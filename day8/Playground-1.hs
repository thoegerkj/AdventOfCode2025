import Data.Vector (Vector, fromList, toList, (!), (//))

type DSU = Vector Set

readPoints :: [String] -> [(Int, Int, Int)]
readPoints = map readPoint
  where
    readPoint :: String -> (Int, Int, Int)
    readPoint s =
      let coords = map read $ split ',' s
       in (coords !! 0, coords !! 1, coords !! 2)
      where
        split :: Char -> String -> [String]
        split c = splitOnC []
          where
            splitOnC acc [] = [reverse acc]
            splitOnC acc (x : xs)
              | x == c = reverse acc : splitOnC [] xs
              | otherwise = splitOnC (x : acc) xs

findEdgeW :: [((Int, Int, Int), Int)] -> [(Double, Int, Int)]
findEdgeW [] = []
findEdgeW (x : xs) = map (distance x) xs ++ findEdgeW xs
  where
    distance :: ((Int, Int, Int), Int) -> ((Int, Int, Int), Int) -> (Double, Int, Int)
    distance ((x1, y1, z1), i1) ((x2, y2, z2), i2) = (sqrt $ fromIntegral ((x1 - x2) ^ 2 + (y1 - y2) ^ 2 + (z1 - z2) ^ 2), i1, i2)

sort :: [(Double, Int, Int)] -> [(Double, Int, Int)]
sort [] = []
sort [x] = [x]
sort (x : xs) =
  let (p1, p2) = partition xs x [] []
      sp1 = sort p1
      sp2 = sort p2
   in sp1 ++ (x : sp2)
  where
    partition :: [(Double, Int, Int)] -> (Double, Int, Int) -> [(Double, Int, Int)] -> [(Double, Int, Int)] -> ([(Double, Int, Int)], [(Double, Int, Int)])
    partition [] x acc1 acc2 = (acc1, acc2)
    partition (t@(d1, _, _) : xs) (d2, _, _) acc1 acc2
      | d2 < d1 = partition xs x acc1 (t : acc2)
      | otherwise = partition xs x (t : acc1) acc2

-- Disjoint Sets
data Set = Set
  { parent :: Int,
    rank :: Int
  }

makeSet :: Int -> Set
makeSet i = Set i 0

findSet :: DSU -> Int -> (Int, DSU)
findSet sets set =
  let Set p r = sets ! set
   in if p == set
        then (set, sets)
        else
          let (rep, nsets) = findSet sets p
              newSet = Set rep r
              nnsets = nsets // [(set, newSet)]
           in (rep, nnsets)

link :: DSU -> Int -> Int -> DSU
link sets s1 s2 =
  let Set p1 r1 = sets ! s1
      Set p2 r2 = sets ! s2
   in if r1 > r2
        then sets // [(p2, Set p1 r2)]
        else
          if r1 < r2
            then sets // [(p1, Set p2 r1)]
            else sets // [(p2, Set p1 r2), (p1, Set p1 (r1 + 1))]

union :: DSU -> Int -> Int -> DSU
union sets s1 s2 = link nsets2 rep1 rep2
  where
    (rep1, nsets1) = findSet sets s1
    (rep2, nsets2) = findSet nsets1 s2

connect :: DSU -> [(Double, Int, Int)] -> Int -> DSU
connect sets _ 0 = sets
connect sets ((_, i1, i2) : edges) iterations =
  let nsets = union sets i1 i2
   in connect nsets edges (iterations - 1)

findLargestComponents :: DSU -> [Int]
findLargestComponents sets =
  let size = length sets
      reps = fromList $ map (fst . findSet sets) [0 .. length sets - 1]
      updateCount counts r = counts // [(r, (counts ! r) + 1)]
      counts =
        foldl
          ( \counts i ->
              let r = reps ! i
               in updateCount counts r
          )
          (fromList (replicate size 0))
          [0 .. length sets - 1]
   in toList counts -- (sortedCounts !! 0, sortedCounts !! 1, sortedCounts !! 2)

sortCounts :: [Int] -> [Int]
sortCounts [] = []
sortCounts [x] = [x]
sortCounts (x : xs) =
  let (p1, p2) = partition xs x [] []
      sp1 = sortCounts p1
      sp2 = sortCounts p2
   in sp1 ++ (x : sp2)
  where
    -- partition :: [Int] -> Int -> [Int] -> [Intnt)] -> ([(Double, Int, Int)], [(Double, Int, Int)])
    partition [] x acc1 acc2 = (acc1, acc2)
    partition (x : xs) p acc1 acc2
      | p < x = partition xs p acc1 (x : acc2)
      | otherwise = partition xs p (x : acc1) acc2

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let points = zip (readPoints $ lines contents) [0 ..]
      edges = findEdgeW points
      sortedEdges = sort edges
      pointSets = fromList [makeSet i | i <- [0 .. length points - 1]]
      iterations = 1000
      components = connect pointSets sortedEdges iterations
      counts = findLargestComponents components
      sortedCounts = reverse $ sortCounts counts
      solution = product $ take 3 sortedCounts

  -- print $ map (fst . findSet components) [0 .. length components - 1]
  -- print counts
  -- print sortedCounts

  -- print c1
  -- print c2
  -- print c3
  print solution
