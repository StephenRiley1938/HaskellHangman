{-# LANGUAGE InstanceSigs #-} 
module A6 where

import Provided

import Data.List(intersperse, sort)
import Data.Char(isLetter, isAlpha, toLower, toUpper)

-- *** A6-0: WARM-UP *** --

-- Q#01
type Chances = Int
type Guess = String
type Move = Char
type Secret = String
type Dictionary = String

-- Q#02

data GameException = InvalidWord | InvalidChars | InvalidLength | NotInDict | RepeatMove | GameOver

-- Q#03

lengthInRange :: Secret -> Bool
lengthInRange ss = l >= fst _LENGTH_ && l <= snd _LENGTH_
  where l = length ss

-- Q#04

invalidMove :: Move -> Bool
invalidMove m = (not . isLetter) m

-- Q#05

revealLetters :: Move -> Secret -> Guess -> Guess
revealLetters m s g = zipWith(\s g -> if s == m && g == '_' then m else g) s g

{-
m = 'b'
secret = "abcdef"
guessIn = "______"
revealLetters 'f' "abcdef" (revealLetters 'c' "abcdef" "______")
-}

-- Q#06
updateChances :: Move -> Secret -> Chances -> Chances
updateChances m s c = if elem m s then c else c - 1

-- Q#07

setSecret :: IO Secret
setSecret = do
  putStr "Enter a secret word:\t"
  showInput False
  secret <- getLine
  showInput True
  _SPACE_
  return secret

-- *** A6-1: Records & Instances *** --

-- Q#08
data Game = Game {  getSecret :: Secret,
                    getCurrentGuess :: Guess,
                    getMovesMade :: [Move],
                    getChancesRemaining :: Chances
                    } -- deriving Show

-- Q#09

repeatedMove :: Move -> Game -> Bool
repeatedMove m g = elem m $ getMovesMade g

-- Q#10

makeGame :: Secret -> Game
makeGame s = Game { getSecret = map toUpper s,
                    getCurrentGuess = replicate (length s) '_',
                    getMovesMade = [],
                    getChancesRemaining = _CHANCES_
                    }

-- Q#11

updateGame :: Move -> Game -> Game
updateGame m g = g {  getCurrentGuess = revealLetters m (getSecret g) (getCurrentGuess g),
                      getMovesMade = m:(getMovesMade g),
                      getChancesRemaining = updateChances m (getSecret g) (getChancesRemaining g)
                      }

-- Q#12
showGameHelper :: String -> [Char] -> Int -> String
showGameHelper game moves chances = unlines [
      _STARS_
    , "\tSecret Word:\t" ++ intersperse ' ' game ++ "\n"
    , "\tGuessed:\t" ++ intersperse ' ' (sort moves) ++ "\n"
    , "\tChances:\t" ++ show chances
    , _STARS_
    ]

instance Show Game where
  show :: Game -> String
  show (Game s _ m c) = showGameHelper s m c

-- Q#13

instance Show GameException where
  show :: GameException -> String

  show InvalidWord = "GameException: InvalidWord."

  show InvalidChars = "GameException: InvalidChars. Must be a letter."

  show InvalidLength = "GameException: InvalidLength. Must be from " ++ lb ++ " to " ++ ub ++ " chracters."
    where
      lb = show $ fst _LENGTH_
      ub = show $ snd _LENGTH_

  show NotInDict = "GameException: NotInDict."

  show RepeatMove = "GameException: RepeatMove."

  show GameOver = "GameException: GameOver."

-- data GameException = InvalidWord | InvalidChars | InvalidLength | NotInDict | RepeatMove | GameOver

-- *** A6-2: Exception Contexts *** --

-- Q#14

toMaybe :: Bool -> a -> Maybe a
toMaybe tf a
    | not tf = Nothing
    | otherwise = Just a

-- Q#15
validateSecret :: (Secret -> Bool) -> GameException-> Secret -> Either GameException Secret
validateSecret f e s = if (f s) then Right s else Left e

-- Q#16

hasValidChars :: Secret -> Either GameException Secret
hasValidChars = validateSecret (\str -> foldl (&&) True (map isLetter str)) InvalidChars

isValidLength :: Secret -> Either GameException Secret
isValidLength = validateSecret lengthInRange InvalidLength

isInDict :: Dictionary -> Secret -> Either GameException Secret
isInDict dict = validateSecret (\str -> foldl (&&) True (map (\c -> (elem c dict)) (map toLower str))) NotInDict

-- Q#17

validateNoDict :: Secret -> Either GameException Secret
validateNoDict s = case hasValidChars s of
                    Right s2 -> case isValidLength s2 of
                                  Right s3 -> Right s3
                                  Left err2 -> Left err2
                    Left err -> Left err

validateWithDict :: Dictionary -> Secret -> Either GameException Secret
validateWithDict d s = case validateNoDict s of
                        Right s2 -> case isInDict d s of
                                      Right s3 -> Right s3
                                      Left err2 -> Left err2
                        Left err -> Left err

-- Q#18

processTurn :: Move -> Game -> Either GameException Game
processTurn m g
  | invalidMove m = Left InvalidChars
  | repeatedMove m g = Left RepeatMove
  | getChancesRemaining newGame == 0 = Left GameOver
  | otherwise = Right newGame
  where newGame = updateGame m g