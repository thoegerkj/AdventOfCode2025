import Data.Set (Set, empty, fromList, insert, member)

splitAtEmptyLine :: [String] -> ([String], [String])
splitAtEmptyLine s = sael s []
  where
    sael :: [String] -> [String] -> ([String], [String])
    -- sael [] acc = (reverse acc, [])
    sael ("" : t) acc = (reverse acc, t)
    sael (h : t) acc = sael t (h : acc)

splitAtDash :: String -> (Int, Int)
splitAtDash s = go s []
  where
    -- go [] acc = (
    go ('-' : t) acc = (read (reverse acc), read t)
    go (h : t) acc = go t (h : acc)

buildSet :: [String] -> Set Int
buildSet strings =
  foldl
    ( \acc1 s ->
        let (n1, n2) = splitAtDash s
         in foldl
              ( \acc2 n ->
                  insert n acc2
              )
              acc1
              [n1 .. n2]
    )
    empty
    strings

countIDs :: [String] -> Set Int -> Int
countIDs s c =
  foldl
    ( \res s ->
        if member (read s) c
          then res + 1
          else res
    )
    0
    s

main :: IO ()
main = do
  contents <- readFile "example.txt"
  let (ranges, ids) = splitAtEmptyLine $ lines contents
      rangeset = buildSet ranges
      freshIdC = countIDs ids rangeset
  print freshIdC
