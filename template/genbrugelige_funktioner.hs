---------------------------------------------------------------------------------------------------------------------------
-- Sorter, bruger quicksort, den er lavet til en tupel med 2 tal, og sorterer efter først, men burde nemt kunne modificeres
---------------------------------------------------------------------------------------------------------------------------
sort :: [(Int, Int)] -> [(Int, Int)]
sort [] = []
sort [x] = [x]
sort (x : xs) =
  let (p1, p2) = partition xs x [] []
      sp1 = sort p1
      sp2 = sort p2
   in sp1 ++ (x : sp2)
  where
    partition :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)] -> ([(Int, Int)], [(Int, Int)])
    partition [] x acc1 acc2 = (acc1, acc2)
    partition (x1 : xs) x acc1 acc2
      | fst x < fst x1 = partition xs x acc1 (x1 : acc2)
      | otherwise = partition xs x (x1 : acc1) acc2

---------------------------------------------------------------------------------------------------------------------------
-- Del en streng i en karakter, ligesom pythons .split(c)
---------------------------------------------------------------------------------------------------------------------------
split :: Char -> String -> [String]
split c = splitOnC []
  where
    splitOnC acc [] = [reverse acc]
    splitOnC acc (x : xs)
      | x == c = reverse acc : splitOnC [] xs
      | otherwise = splitOnC (x : acc) xs

---------------------------------------------------------------------------------------------------------------------------
-- put . hele vejen rundt om en matrix a char
---------------------------------------------------------------------------------------------------------------------------
pad :: [String] -> [String]
pad lines =
  let height = length lines
      width = length (head lines)
      emptyRow = replicate (width + 2) '.'
      paddedLines = map (\line -> '.' : line ++ ".") lines
   in emptyRow : paddedLines ++ [emptyRow]
