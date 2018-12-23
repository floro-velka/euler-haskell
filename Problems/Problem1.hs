calculate :: (Integral a) => a -> a
calculate x
    | x < 0     = error "Invalid."
    | otherwise =
        let isMultOf3or5 a = a `mod` 3 == 0 || a `mod` 5 == 0
            threeFiveMult  = filter isMultOf3or5 [1..x]
        in  sum threeFiveMult

main = do
    print $ calculate 999