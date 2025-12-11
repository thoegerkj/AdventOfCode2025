import Debug.Trace (trace)

----------------------------------------------
-- Some helper functions
split :: Char -> String -> [String]
split c = splitOnC []
  where
    splitOnC acc [] = [reverse acc]
    splitOnC acc (x : xs)
      | x == c = reverse acc : splitOnC [] xs
      | otherwise = splitOnC (x : acc) xs

insideBraces :: Char -> Char -> String -> (String, String)
insideBraces open close (o : s)
  | o == open = go s []
  where
    go (x : xs) acc
      | x == close = (reverse acc, xs)
      | x == open = go xs acc
      | otherwise = go xs (x : acc)

----------------------------------------------
-- Actual solution
----------------------------------------------

type MachineSpec = (String, [[Int]], [Int])

-- Read input
readMachineSpec :: String -> MachineSpec
readMachineSpec = joltages . buttons . lights
  where
    lights :: String -> (String, String)
    lights (']' : rest) = ("", rest)
    -- lights ('[':xs) = lights xs
    lights (x : xs)
      | x `elem` ['.', '#'] = let (ls, rest) = lights xs in (x : ls, rest)
      | otherwise = lights xs

    buttons :: (String, String) -> (String, [[Int]], String)
    buttons (ls, ' ' : xs) = buttons (ls, xs) -- ignore spaces
    buttons (ls, rest@('{' : _)) = (ls, [], rest)
    buttons (ls, xs) =
      let (btn, rest) = button xs
          (nls, btns, finalRest) = buttons (ls, rest)
       in (nls, btn : btns, finalRest)
      where
        button :: String -> ([Int], String)
        button s =
          let (numsStr, rest) = insideBraces '(' ')' s
           in (map read $ split ',' numsStr, rest)

    joltages :: (String, [[Int]], String) -> MachineSpec
    joltages (ls, btns, rest) =
      let (numsStr, _) = insideBraces '{' '}' rest
       in (ls, btns, map read $ split ',' numsStr)

-- Actual actual solution
fewestPresses :: MachineSpec -> Int
fewestPresses (ls, btns, _) = tryAllButtons btns 1
  where
    tryAllButtons :: [[Int]] -> Int -> Int
    tryAllButtons btns presses =
      if possible ls btns presses []
        then presses
        else tryAllButtons btns (presses + 1)

    possible :: String -> [[Int]] -> Int -> [[Int]] -> Bool
    possible ls _ 0 acc = all (== '.') ls
    possible ls btns presses acc =
      foldl
        (\possibleYet btn -> possibleYet || possible (toggle ls btn 0) btns (presses - 1) (btn : acc))
        False
        btns

    toggle :: String -> [Int] -> Int -> String
    toggle ls [] index = ls
    toggle (l : ls) (b : bs) index
      | index == b =
          let nl = if l == '.' then '#' else '.'
           in nl : toggle ls bs (index + 1)
      | otherwise = l : toggle ls (b : bs) (index + 1)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let machines = lines contents
      specs = map readMachineSpec machines
      result = sum $ map fewestPresses specs
  -- print specs
  print result
