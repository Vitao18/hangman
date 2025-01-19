module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse, intercalate)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

type WordList = [String]

data Puzzle = Puzzle
  { wordToGuess :: String
  , filledInSoFar :: [Maybe Char]
  , guessed :: [Char]
  , missed :: Int
  }

instance Show Puzzle where
    show (Puzzle {filledInSoFar=discovered, guessed=guessed, missed=missed}) =
        (intersperse ' ' $ fmap renderPuzzleChar discovered)
        ++ " Guessed so far: " ++ guessed ++ "\n" ++ (hangmanAscii missed) ++ "\n"

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter puzzle@(Puzzle
                 { wordToGuess=word
                 , filledInSoFar=filledInSoFar
                 , guessed=s
                 }) c = puzzle
                        { filledInSoFar=newFilledInSoFar
                        , guessed=c:s
                        }
    where
        zipper guessed wordChar guessChar =
            if wordChar == guessed then Just wordChar else guessChar
        newFilledInSoFar =
            zipWith (zipper c) word filledInSoFar

fillCorrect :: Puzzle -> Char -> Puzzle
fillCorrect = fillInCharacter

fillIncorrect :: Puzzle -> Char -> Puzzle
fillIncorrect puzzle@(Puzzle {missed = missed}) = fillInCharacter newPuzzle
  where newPuzzle = puzzle {missed=missed + 1}

freshPuzzle :: String -> Puzzle
freshPuzzle word = Puzzle word (map (const Nothing) word) [] 0

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle {wordToGuess=word}) x = elem x word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle {guessed=guessed}) x = elem x guessed

allWords :: IO WordList
allWords = do
    dict <- readFile "data/dict.txt"
    return (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
    aw <- allWords
    return (filter gameLength aw)
    where gameLength w = ((length w) > minWordLength) && ((length w) < maxWordLength)

hangmanAscii :: Int -> String
hangmanAscii = (!!) asciiArt
  where asciiArt = map (intercalate "\n") $
          [
            [ "   _______  "
            , "   | /   |  "
            , "   |/       "
            , "   |        "
            , "   |        "
            , "   |        "
            , "___|___     "
            ]

          , [ "   _______  "
            , "   | /   |  "
            , "   |/    O  "
            , "   |        "
            , "   |        "
            , "   |        "
            , "___|___     "
            ]

          , [ "   _______  "
            , "   | /   |  "
            , "   |/    O  "
            , "   |     |  "
            , "   |     |  "
            , "   |        "
            , "___|___     "
            ]

          , [ "   _______  "
            , "   | /   |  "
            , "   |/    O  "
            , "   |    \\|  "
            , "   |     |  "
            , "   |        "
            , "___|___     "
            ]

          , [ "   _______  "
            , "   | /   |  "
            , "   |/    O  "
            , "   |    \\|/ "
            , "   |     |  "
            , "   |        "
            , "___|___     "
            ]

          , [ "   _______  "
            , "   | /   |  "
            , "   |/    O  "
            , "   |    \\|/ "
            , "   |     |  "
            , "   |    /   "
            , "___|___     "
            ]

          , [ "   _______  "
            , "   | /   |  "
            , "   |/    O  "
            , "   |    \\|/ "
            , "   |     |  "
            , "   |    / \\"
            , "___|___     "
            ]
          ]

randomWord :: WordList -> IO String
randomWord wl = do
    index <- randomRIO (0, (length wl) - 1)
    return $ wl !! index

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
    putStrLn $ "Your guess was: " ++ [guess]
    case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
      (_, True) -> do
        putStrLn "You already guessed that character, pick something else!"
        return puzzle
      (True, _) -> do
        putStrLn "This character was in the word, filling in the word accordingly."
        return . fillCorrect puzzle $ guess
      (False, _) -> do
        putStrLn "This character wasn't in the word, try again."
        return . fillIncorrect puzzle $ guess

gameOver :: Puzzle -> IO ()
gameOver (Puzzle
          { wordToGuess=wordToguess
          , guessed=guessed
          , missed=missed
          }) =
    if missed > 5 then
        do
            putStrLn . hangmanAscii $ missed
            putStrLn "You lose!"
            putStrLn $ "The word was: " ++ wordToguess
            exitSuccess
    else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle {filledInSoFar=filledInSoFar}) =
    if all isJust filledInSoFar then
        do
            putStrLn "You win!"
            exitSuccess
    else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
    gameOver puzzle
    gameWin puzzle
    putStrLn $ "Current puzzle is: " ++ show puzzle
    putStr "Guess a letter: "
    guess <- getLine
    case guess of
        [c] -> handleGuess puzzle c >>= runGame
        _ -> putStrLn "Your guess must be a single character"

main :: IO ()
main = do
    wl <- gameWords
    word <- randomWord wl
    let puzzle = freshPuzzle (fmap toLower word)
    runGame puzzle
