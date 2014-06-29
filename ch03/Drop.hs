module Drop where
    niceDrop :: Integer -> [a] -> [a]
    niceDrop n xs | n <= 0 = xs
    niceDrop _ [] = []
    niceDrop n (_:xs) = niceDrop (n - 1) xs
