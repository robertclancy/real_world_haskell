module Direction where
    import Data.Complex
    import Data.Ord
    import Data.List

    data Direction = Left | Right | Collinear deriving (Eq, Show)

    data Point2 = Point2 Double Double deriving (Eq, Show)

    pointwise :: (Double -> Double -> Double) -> (Point2 -> Point2 -> Point2)
    pointwise f (Point2 x y) (Point2 u v) = Point2 (f x u) (f y v)

    add :: Point2 -> Point2 -> Point2
    add = pointwise (+)

    sub :: Point2 -> Point2 -> Point2
    sub = pointwise (-)

    angle :: Point2 -> Point2 -> Point2 -> Direction
    angle x y z = angle2 (sub y x) (sub z x)

    angle2 :: Point2 -> Point2 -> Direction
    angle2 x y = case compare (imagPart ((toComplex x) / (toComplex y))) 0 of
                     GT -> Direction.Left
                     LT -> Direction.Right
                     EQ -> Collinear

    toComplex :: Point2 -> Complex Double
    toComplex (Point2 x y) = x :+ y

    apply3 :: (a -> a -> a -> b) -> [a] -> [b]
    apply3 f (x:y:z:xs) = (f x y z):(apply3 f (y:z:xs))
    apply3 _ _        = []

    directions :: [Point2] -> [Direction]
    directions = apply3 angle

    graham_scan :: [Point2] -> [Point2]
    graham_scan xs = map fst (filter ((==) Direction.Right . snd) (zip sortedByAngle (Direction.Right : (directions sortedByAngle) ++ [Direction.Right]))) 
                     where sortedByAngle = sortBy (comparing (phaseWithPoint (origin xs))) xs
                           phaseWithPoint x y = shiftInterval (phase ((toComplex x) - (toComplex y)))
                           shiftInterval x
                            | x >=0 = x
                            | otherwise = x + 2 * pi

    origin :: [Point2] -> Point2
    origin = minimumBy (comparing yThenX)
             where
                 yThenX (Point2 x y) = [x, y]





