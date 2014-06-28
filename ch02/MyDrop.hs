-- ch02/MyDrop.hs
module MyDrop where
myDrop :: Integer -> [a] -> [a]
myDrop n xs = if n < 0 || null xs
                  then xs
                  else myDrop (n - 1) (tail xs)

lastButOne :: [a] -> a
lastButOne xs = last (take ((length xs) - 1) xs)
