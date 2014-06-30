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
