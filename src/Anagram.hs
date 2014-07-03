module Anagram (anagram) where

    import Control.Monad.State
    import Control.Monad.List
    import Control.Monad.Identity

    type Ana = StateT Src []
    type Src = String
    type Tar = String

    anagramHelper :: Tar -> Ana Tar
    anagramHelper tar = do
        src <- get
        (src', c) <- lift $ pick src
        if isEmpty src'
            then return (tar ++ [c])
            else do
                put src'
                anagramHelper (tar ++ [c])

    isEmpty :: [a] -> Bool
    isEmpty [] = True
    isEmpty _ = False

    pick :: Src -> [(Src, Char)]
    pick src = do
        c <-  src
        return (removeOne c src, c)

    removeOne :: Char -> Src -> Src
    removeOne c src = takeWhile (/= c) src ++ tail (dropWhile (/= c) src)

    anagram :: String -> [String]
    anagram src = evalStateT (anagramHelper "") src

