import Data.Map qualified as Map

-- import Debug.Trace (trace)

makeIList :: String -> ([[Int]], Int, Int, Int)
makeIList s =
  let devices = lines s
      n = length devices - 1
      sToIndex =
        foldl
          ( \acc (line, i) ->
              let ws = words line
                  device = take 3 (head ws) -- drop the trailing :
               in Map.insert device i acc
          )
          Map.empty
          (zip (devices ++ ["out"]) [0 .. n + 1])
   in ( reverse $
          foldl
            ( \acc i ->
                let device = words $ devices !! i
                    connections = drop 1 device
                    connectionAsI = map (sToIndex Map.!) connections
                 in connectionAsI : acc
            )
            []
            [0 .. n],
        sToIndex Map.! "svr",
        sToIndex Map.! "dac",
        sToIndex Map.! "fft"
      )

sortAndReverse :: [(Int, [Int])] -> [Int]
sortAndReverse ilist =
  let removeFromList n (i, []) = (i, [])
      removeFromList n (i, x : xs)
        | n == x = removeFromList n (i, xs)
        | otherwise = let (_, nxs) = removeFromList n (i, xs) in (i, x : nxs)

      removeFromIList ilist' n = map (removeFromList n) ilist'

      findAllDevicesWithOutdegree0 [] = []
      findAllDevicesWithOutdegree0 ((i, x) : xs)
        | null x = (i, x) : findAllDevicesWithOutdegree0 xs
        | otherwise = findAllDevicesWithOutdegree0 xs

      buildTopSort [] acc = acc
      buildTopSort ilist' acc =
        let nextToAdd = findAllDevicesWithOutdegree0 ilist'
            removedIList = filter (not . null . snd) ilist'
            nilist =
              foldl
                (\acc (n, _) -> removeFromIList acc n)
                removedIList
                nextToAdd
         in buildTopSort nilist (map fst nextToAdd ++ acc)

      ilistWithOut = ilist ++ [(length ilist, [])] -- add the out entry
   in reverse $ buildTopSort ilistWithOut []

dp :: [Int] -> [[Int]] -> Int -> Int -> [(Int, Int, Int, Int)]
dp topsort ilist iod iof = recurse topsort (length topsort - 1)
  where
    recurse ts 0 = [(1, 0, 0, 0)]
    recurse ts i =
      let memo = recurse ts (i - 1)
          v = ts !! i
          isIOD = i == iod
          isIOF = i == iof
          neighbors = ilist !! v
          placements = map (\ns -> toIndexInTS ns 0 ts) neighbors
          w = map (memo !!) placements
          (rw1, rw2, rw3, rw4) =
            foldl
              ( \(nw1, nw2, nw3, nw4) (w1, w2, w3, w4) ->
                  ( if not (isIOD || isIOF) then nw1 + w1 else 0,
                    if isIOD then nw2 + w1 else nw2 + w2,
                    if isIOF then nw3 + w1 else nw3 + w3,
                    nw4 + w4
                  )
              )
              (0, 0, 0, 0)
              w
          lw =
            if isIOD
              then rw3
              else
                if isIOF
                  then rw2
                  else rw4

          toIndexInTS :: Int -> Int -> [Int] -> Int
          toIndexInTS n i (x : xs)
            | n == x = i
            | otherwise = toIndexInTS n (i + 1) xs
       in memo ++ [(rw1, rw2, rw3, lw)]

find :: Int -> [Int] -> Int -> Int
find you (t : ts) i
  | you == t = i
  | otherwise = find you ts (i + 1)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let (ilist, ios, iod, iof) = makeIList input
      reverseSortedDag = sortAndReverse (zip [0 ..] ilist)
      iosid = find ios reverseSortedDag 0
      iodid = find iod reverseSortedDag 0
      iofid = find iof reverseSortedDag 0
      solutions = dp reverseSortedDag ilist iodid iofid
      -- solutionsWC
      solution = solutions !! iosid
  -- print ilist
  -- print indexOfYou
  -- print reverseSortedDag
  -- print iosid
  -- print iodid
  -- print iofid

  -- print solutions
  print solution

-- print solution
