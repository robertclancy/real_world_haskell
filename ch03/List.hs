module List where
    data List a = Cons a (List a)
                | Nil
                  deriving (Show)

    fromList :: [a] -> List a
    fromList = foldr Cons Nil

    toList :: List a -> [a]
    toList (Cons x xs) = x : toList xs
    toList Nil         = []

