module Anagram (anagram) where

    import Control.Monad.State
    import Control.Monad.List
    import Control.Monad.Identity

    type Ana = StateT (Src, Tar) []
    type Src = String
    type Tar = String

    anagramHelper :: Ana Tar
    anagramHelper = do
        (src, tar) <- get
        (src', c) <- lift $ pick src
        put (src', tar ++ [c])
        if isEmpty src'
            then return (tar ++ [c])
            else anagramHelper

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
    anagram src = evalStateT anagramHelper (src,"")

    