findBiggestNumberAndIndex :: String -> (Int, Int)
findBiggestNumberAndIndex battery =
  foldl
    ( \(maxNum, maxIdx) (c, idx) ->
        let num = read [c] :: Int
         in if num > maxNum
              then (num, idx)
              else (maxNum, maxIdx)
    )
    (-1, -1)
    (zip battery [0 ..])

biggestNumber :: String -> Int
biggestNumber battery =
  let (n1, i1) = findBiggestNumberAndIndex $ init battery
      (_, restOfBattery) = splitAt (i1 + 1) battery
      (n2, _) = findBiggestNumberAndIndex restOfBattery
   in read (show n1 ++ show n2) :: Int

main :: IO ()
main = do
  contents <- readFile "input.txt"
  print $
    foldl
      ( \acc line ->
          acc + biggestNumber line
      )
      0
      (lines contents)

-- Additional lobby logic can be added here
