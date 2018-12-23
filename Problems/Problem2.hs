calculate :: (Integral a) => a
calculate = fib 1 2 0 4000000
    where fib x y sum max
            | x > max = sum
            | otherwise   = fib y (x+y) (if x `mod` 2 == 0 then sum+x else sum) max

main = do
    print $ calculate