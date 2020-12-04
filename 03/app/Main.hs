module Main where

type PasswordEntry = (Int, Int, Char, String)

main :: IO ()
main = (fmap lines . readFile $ "./input.txt") >>=
  (\lines -> return $ fmap lineToEntry lines) >>=
  (\entries -> return $ filter validPasswordEntry entries) >>=
  (\valid -> return $ length valid) >>=
  (\amount -> putStrLn $ show amount)

validPasswordEntry :: PasswordEntry -> Bool
validPasswordEntry (min, max, requiredChar, password) = occurrences >= min && occurrences <= max
  where
    occurrences = length (filter (\x -> x == requiredChar) password)

splitByPredicate :: (Char -> Bool) -> String -> [String]
splitByPredicate pred input = case dropWhile pred input of
  "" -> []
  input' -> w : splitByPredicate pred input''
    where (w, input'') = break pred input'

lineToEntry :: String -> PasswordEntry
lineToEntry line = (minCount, maxCount, requiredChar, password)
  where
    formattingChars = ['-', ' ', ':']
    [minCount', maxCount', (requiredChar:_), password] = splitByPredicate (\x -> elem x formattingChars) line
    minCount = read minCount' :: Int
    maxCount = read maxCount' :: Int
