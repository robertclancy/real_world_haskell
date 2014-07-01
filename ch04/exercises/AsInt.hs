module AsInt where
    import Data.Char
    import Data.List

    asInt :: String -> Int
    asInt = asSignedInt
            where asSignedInt y@(x:xs) | x == '-'  = - asPositiveInt xs
                                       | otherwise = asPositiveInt y
                  asSignedInt [] = 0
                  asPositiveInt = foldl' step 0
                                  where step acc x = (10 * acc) + digitToInt x

    type ErrorMessage = String
    asIntEither :: String -> Either ErrorMessage Int
    asIntEither = asSignedInt
                  where asSignedInt y@(x:xs) | x == '-'  = do i <- asPositiveInt xs
                                                              return (-i)
                                             | otherwise = asPositiveInt y
                        asSignedInt [] = Left "Empty string"
                        asSignedInt :: String -> Either ErrorMessage Int
                        asPositiveInt = foldl' step (Right 0)
                                        where step acc x = do
                                                              acc' <- acc
                                                              digit <- asDigit x
                                                              return ((10 * acc') + digit)
                        asDigit :: Char -> Either ErrorMessage Int
                        asDigit '0' = Right 0
                        asDigit '1' = Right 1
                        asDigit '2' = Right 2
                        asDigit '3' = Right 3
                        asDigit '4' = Right 4
                        asDigit '5' = Right 5
                        asDigit '6' = Right 6
                        asDigit '7' = Right 7
                        asDigit '8' = Right 8
                        asDigit '9' = Right 9
                        asDigit x   = Left ("Char: " ++ [x] ++ " is not a digit.")
