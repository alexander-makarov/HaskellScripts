import Data.Char

sumCows :: (Eq a, Num b) => [a] -> [a] -> b
sumCows [] _ = 0
sumCows _ [] = 0
sumCows (x:xs) (y:ys) = (if (x /= y) && (elem y xs) then 1 else 0) + sumCows (xs++[x]) ys 

capitalize :: String -> String
capitalize = map toUpper

containsDuplicates :: (Eq a) => [a] -> Bool
containsDuplicates [] = False
containsDuplicates [x] = False
containsDuplicates (x:xs) = (elem x xs) || containsDuplicates xs

playTheGame :: String -> Int -> IO ()
playTheGame isogramWord attemptsNum
  | attemptsNum <= 0 = putStrLn "Jeez, you should allow for at least one attempt to guess.."
  | containsDuplicates $ capitalize isogramWord = putStrLn "You play Bulls'n'Cows only with an isogram words!"
  | otherwise = guessTurn isogramWord attemptsNum

guessTurn :: String -> Int -> IO ()
guessTurn _ attemptsNum
  | attemptsNum <= 0 = putStrLn "Too bad, no more attempts left. You lost!"
guessTurn isogramWord attemptsNum = 
  do putStrLn ("You have "++ (show attemptsNum) ++" attemps left. Make your guess: " ++ ['#' | _ <- isogramWord])
     inputGuess <- getLine
     let isogramWord' = capitalize isogramWord
     let inputGuess' = capitalize inputGuess
     let bulls = length $ filter (== True) $ zipWith (==) isogramWord' inputGuess'
     let cows = sumCows isogramWord' inputGuess'
     putStrLn ("You've got " ++ (show bulls) ++ " bulls and " ++ (show cows) ++ " cows.")
     if bulls == (length isogramWord)     
        then putStrLn "Attaboy! You've just won!"
        else guessTurn isogramWord (attemptsNum-1)