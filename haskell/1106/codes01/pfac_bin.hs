module Main (
    main, fancyString
) where

import System.Environment (getArgs)
import Primes (factorize)

main = do
    c <- getArgs
    putStrLn (fancyString (factorize (read (head c))))

fancyString []      = ""
fancyString (p:[])  = show p
fancyString (p:ps)  = show p ++ " x " ++ fancyString ps
