module Main where

type PasswordEntry = (Int, Int, Char, String)

main :: IO ()
main = (fmap lines . readFile $ "./input.txt") >>=
  (\lines -> return $ fmap lineToEntry lines) >>=
  (\entries -> return $ filter validPasswordEntry entries) >>=
  (\valid -> return $ length valid) >>=
  (\amount -> putStrLn $ show amount)

validPasswordEntry :: PasswordEntry -> Bool
validPasswordEntry (indexOne, indexTwo, requiredChar, password) = (firstRight && not secondRight) || (not firstRight && secondRight)
  where
    firstRight = password !! (indexOne - 1) == requiredChar
    secondRight = password !! (indexTwo - 1) == requiredChar

splitByPredicate :: (Char -> Bool) -> String -> [String]
splitByPredicate pred input = case dropWhile pred input of
  "" -> []
  input' -> w : splitByPredicate pred input''
    where (w, input'') = break pred input'

lineToEntry :: String -> PasswordEntry
lineToEntry line = (indexOne, indexTwo, requiredChar, password)
  where
    formattingChars = ['-', ' ', ':']
    [indexOne', indexTwo', (requiredChar:_), password] = splitByPredicate (\x -> elem x formattingChars) line
    indexOne = read indexOne' :: Int
    indexTwo = read indexTwo' :: Int
