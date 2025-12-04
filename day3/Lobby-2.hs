dropLastN :: String -> Int -> String
dropLastN str n = take (length str - n) str

findBiggestNumberAndIndex :: String -> (String, Int)
findBiggestNumberAndIndex battery =
  let (maxNum, maxIdx) =
        foldl
          ( \(maxNum, maxIdx) (c, idx) ->
              let num = read [c] :: Int
               in if num > maxNum
                    then (num, idx)
                    else (maxNum, maxIdx)
          )
          (-1, -1)
          (zip battery [0 ..])
   in (show maxNum, maxIdx)

biggestNumber :: String -> Int
biggestNumber battery =
  let (result, _) =
        foldl
          ( \(res, restOfBattery) _ ->
              let (n, i) = findBiggestNumberAndIndex $ dropLastN restOfBattery (11 - length res)
                  (_, newRestOfBattery) = splitAt (i + 1) restOfBattery
               in (n ++ res, newRestOfBattery)
          )
          ("", battery)
          [1 .. 12]
   in read (reverse result) :: Int

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
