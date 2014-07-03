-- | Main entry point to the application.
module Main where

    import Anagram
    import MultiSet (remDup)
    import Control.Applicative

    main :: IO ()
    main = fmap (remDup . anagram) getLine >>= printList

    printList :: [String] -> IO ()
    printList = foldr ((>>) . putStrLn) (return ())