split :: Char -> String -> [String]
split c = splitOnC []
  where
    splitOnC acc [] = [reverse acc]
    splitOnC acc (x : xs)
      | x == c = reverse acc : splitOnC [] xs
      | otherwise = splitOnC (x : acc) xs

checkIfWW :: String -> Bool
checkIfWW s =
  let checkEquals :: [String] -> Bool
      checkEquals (x : xs) = all (== x) xs

      splitAtEvery :: Int -> String -> [String]
      splitAtEvery l s
        | length s <= l = [s]
        | otherwise = let (first, rest) = splitAt l s in first : splitAtEvery l rest

      checkForNumber :: String -> Int -> Bool
      checkForNumber str n = checkEquals $ splitAtEvery (length str `div` n) str
   in foldl
        (\acc n -> checkForNumber s n || acc)
        False
        [2 .. length s]

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

checkRanges :: [String] -> Int
checkRanges =
  let splitIn2 :: Char -> String -> (String, String)
      splitIn2 c s =
        let x : xs = split c s
         in (x, last xs)
   in foldl
        ( \acc range ->
            let (aStr, bStr) = splitIn2 '-' range
                a = read aStr :: Int
                b = read bStr :: Int
             in acc + checkRange a b
        )
        0

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let ranges = split ',' contents

  print $ show $ checkRanges ranges
