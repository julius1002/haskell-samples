import Data.Text (pack, replace, unpack)

hangman :: IO ()
hangman = do
  putStrLn "Think of a word:"
  word <- getLine
  putStrLn "Try to guess it:"
  play word

play :: [Char] -> IO ()
play [] = do
  putStrLn "Enter another word"
  otherWord <- getLine
  play otherWord
play word = do
  guess <- getLine
  if guess == word
    then putStrLn "You Won"
    else do
      putStrLn "Wrong word"
      putStrLn word
      play word


--replaceCorrectLetters :: [Char] -> [Char] -> [Char]
--replaceCorrectLetters word guess = unpack . replace guess guess . pack