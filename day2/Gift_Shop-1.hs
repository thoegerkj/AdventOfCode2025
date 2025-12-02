split :: Char -> String -> [String]
split c = splitOnC []
  where
    splitOnC acc [] = [reverse acc]
    splitOnC acc (x : xs)
      | x == c = reverse acc : splitOnC [] xs
      | otherwise = splitOnC (x : acc) xs

checkIfWW :: String -> Bool
checkIfWW s =
  let checkEquals :: (String, String) -> Bool
      checkEquals (a, b) = a == b
   in checkEquals $ splitAt (length s `div` 2) s

checkRange :: Int -> Int -> Int
checkRange a b =
  foldl
    ( \acc x ->
        if checkIfWW $ show x
          then acc + x
          else acc
    )
    0
    [a .. b]

splitIn2 :: Char -> String -> (String, String)
splitIn2 c s =
  let x : xs = split c s
   in (x, last xs)

checkRanges :: [String] -> Int
checkRanges ranges =
  foldl
    ( \acc range ->
        let (aStr, bStr) = splitIn2 '-' range
            a = read aStr :: Int
            b = read bStr :: Int
         in acc + checkRange a b
    )
    0
    ranges

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let ranges = split ',' contents

  print $ show $ checkRanges ranges
