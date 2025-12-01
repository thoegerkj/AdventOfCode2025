main = do
  let fib x = if x < 2 then x else fib (x - 1) + fib (x - 2)
  putStrLn ("Hello! fib 20: " ++ show (fib 20))
