module MultiSet where

    type Multi a = [(a,Int)]

    toMulti :: Eq a => [a] -> Multi a
    toMulti [] = []
    toMulti (x:xs) = (x, length isX) : toMulti notX where
        isX = filter (x ==) xs
        notX = filter (x /=) xs

    toSet :: Eq a => Multi a -> [a]
    toSet [] = []
    toSet ((x,n):xs) = x : toSet xs

    remDup :: Eq a => [a] -> [a]
    remDup = (toSet . toMulti)