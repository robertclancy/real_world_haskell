module Lists where
    import Data.List (sortBy)

    myLength :: [a] -> Integer
    myLength = foldr (const succ) 0

    mean :: Fractional a => [a] -> a
    mean xs = (sum xs) / (fromIntegral (length xs))

    palindrome :: [a] -> [a]
    palindrome xs = xs ++ reverse xs

    is_palindrome :: (Eq a) => [a] -> Bool
    is_palindrome xs = xs == reverse xs

    sortByLength :: [[a]] -> [[a]]
    sortByLength = sortBy (\x y -> compare (length x) (length y))

    _intersperse :: a -> [[a]] -> [a]
    _intersperse x = foldr inter []
        where inter y [] = y
              inter [] z = z
              inter y  z = y ++ (x:z)
