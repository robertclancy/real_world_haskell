module Lists where
    safeHead :: [a] -> Maybe a
    safeHead [] = Nothing
    safeHead (x:_) = Just x

    safeTail :: [a] -> Maybe [a]
    safeTail [] = Nothing
    safeTail (_:xs) = Just xs

    safeLast :: [a] -> Maybe a
    safeLast = safeHead . reverse

    safeInit :: [a] -> Maybe [a]
    safeInit = (fmap reverse) . safeTail . reverse

    splitWith :: (a -> Bool) -> [a] -> [[a]]
    splitWith _ [] = []
    splitWith f xs = let (pre, suf) = span f xs in
                     pre : splitWith f (dropWhile (not . f) suf)

    concatUsingFold :: [[a]] -> [a]
    concatUsingFold = foldr (++) []

    -- The laziness of Haskell is important here
    takeWhileUsingFold :: (a -> Bool) -> [a] -> [a]
    takeWhileUsingFold p = foldr takeNext []
                           where takeNext x acc | p x       = x : acc
                                                | otherwise = []

    groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
    groupBy p xs = foldr insertElement [] xs
                   where insertElement x (y:ys) | p x (head y) = (x:y):ys
                                                | otherwise    = y : insertElement x ys
                         insertElement x [] = [[x]]
