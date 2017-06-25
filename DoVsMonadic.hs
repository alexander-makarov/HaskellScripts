-- http://learnyouahaskell.com/a-fistful-of-monads
module Main where
import System.Environment
main::IO()

-- 			do notation:
--main = do
--	a <- getLine
--	putStrLn ("Hello " ++ a )


-- 			monadic notation:
main = 
	getLine >>= (\a -> 
	putStrLn ("Hello " ++ a ))