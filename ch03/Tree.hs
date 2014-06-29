module Tree where
    data Tree a = Node a (Tree a) (Tree a)
                | Empty
                  deriving (Show)
    data OtherTree a = OtherTree a (Maybe (OtherTree a)) (Maybe (OtherTree a))
