import qualified Data.Map as Map
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
readMachineSpec :: String -> ([[Int]], [Int])
readMachineSpec = toMatrix . joltages . buttons . lights
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

    toMatrix :: MachineSpec -> ([[Int]], [Int])
    toMatrix (ls, btns, js) =
      let m = length ls
          btnM = map (\b -> map (\i -> if i `elem` b then 1 else 0) [0 .. m - 1]) btns
       in (btnM, js)

-- Actual actual solution
fewestPresses :: ([[Int]], [Int]) -> Int
fewestPresses (btns, jolts) = trace "Entered function fewestPresses" fst $ tryAll jolts Map.empty
  where
    tryAll :: [Int] -> Map.Map [Int] Int -> (Int, Map.Map [Int] Int)
    tryAll js memo
      | Map.member js memo = (memo Map.! js, memo)
      | any (< 0) js = trace ("invalid found: " ++ show js) (maxBound :: Int, Map.insert js (maxBound :: Int) memo)
      | all (== 0) js = trace ("found solution: " ++ show js) (0, Map.insert js 0 memo)
      | otherwise =
          -- trace ("trying from state: " ++ show js) $
          foldl
            ( \(minPresses, curMemo) btn ->
                let (presses, newMemo) = tryAll (toggle js 0 btn) curMemo
                 in (min minPresses (presses + 1), newMemo)
            )
            (maxBound :: Int, memo)
            btns

    toggle :: [Int] -> Int -> [Int] -> [Int]
    toggle js _ [] = js
    toggle (l : ls) index (b : bs)
      | b == 1 =
          let nl = l - 1
           in nl : toggle ls (index + 1) bs
      | otherwise =
          l : toggle ls (index + 1) bs

main :: IO ()
main = do
  contents <- readFile "example.txt"
  let machines = lines contents
      matrices = map readMachineSpec machines
      result = map fewestPresses matrices
  print result

-- print result
