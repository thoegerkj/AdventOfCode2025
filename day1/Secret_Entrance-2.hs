leftrotate :: Int -> Int -> Int -> (Int, Int)
leftrotate n pos num = foldl (\(res, p) _ -> (if (p - 1) `mod` 100 == 0 then res + 1 else res, p - 1)) (n, pos) [1 .. num]

rightrotate :: Int -> Int -> Int -> (Int, Int)
rightrotate n pos num = foldl (\(res, p) _ -> (if (p + 1) `mod` 100 == 0 then res + 1 else res, p + 1)) (n, pos) [1 .. num]

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let (zeroes, final_pos) =
        foldl
          ( \(acc, pos) rot ->
              let dir = head rot
                  num = read $ tail rot :: Int
               in if dir == 'L'
                    then leftrotate acc pos num
                    else rightrotate acc pos num
          )
          (0, 50)
          (lines contents)

  print zeroes
  print "too high: 5900"
