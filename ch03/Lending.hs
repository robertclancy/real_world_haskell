module Lending where
    lend  :: Int -> Int -> Maybe Int
    lend amount balance = let reserve = 100
                              newBalance = balance - amount
                              in if balance < reserve
                                     then Nothing
                                     else Just newBalance

    lend2  :: Int -> Int -> Maybe Int
    lend2 amount balance = if balance < reserve
                               then Nothing
                               else Just newBalance
                               where reserve = 100
                                     newBalance = balance - amount

    lend3 :: Int -> Int -> Maybe Int
    lend3 amount balance
        | balance < reserve = Nothing
        | otherwise = Just newBalance
        where reserve = 100
              newBalance = balance - amount
