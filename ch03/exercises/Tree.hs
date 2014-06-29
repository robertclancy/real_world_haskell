module Tree where
    data Tree a = Node a (Tree a) (Tree a)
                | Empty
                  deriving (Show)
    
    height :: Tree a -> Integer
    height Empty = 0
    height (Node _ x y) = succ (max (height x) (height y))
