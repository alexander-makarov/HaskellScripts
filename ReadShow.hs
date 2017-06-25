-- https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours
module Main where
import System.Environment
main::IO()
main = do
	args <- getArgs
	let a = read $ args !! 0
	let b = read $ args !! 1	
	putStrLn ("Result=" ++ show(a+b) )