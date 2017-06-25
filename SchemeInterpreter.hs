-- https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours
module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse (spaces >> symbol) "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             
--We're back to using the do-notation instead of the >> operator. 
--This is because we'll be retrieving the value of our parse (returned by many(noneOf "\"")) and manipulating it, interleaving some other parse operations in the meantime. 
--In general, use >> if the actions don't return a value, >>= if you'll be immediately passing that value into the next action, and do-notation otherwise.
parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many (noneOf "\"")
                char '"'
                return $ String x

main :: IO ()
main = do 
         (expr:_) <- getArgs
         putStrLn (readExpr expr)