module A7 where

import Provided
import A6

import Data.Char ( toUpper )
import System.Directory ( doesFileExist )

-- *** A7: Functors & Applicatives *** --

-- Q#01

getUpperChar :: IO Char
--getUpperChar = getChar >>= (\c -> return (toUpper c))
--getUpperChar = fmap toUpper getChar
getUpperChar = toUpper <$> getChar

-- Q#02

_DICT_ :: IO [String]
--_DICT_ = readFile _DICT_FILE_ >>= (\w -> return (words w))
--_DICT_ = fmap words (readFile _DICT_FILE_)
_DICT_ = words <$> readFile _DICT_FILE_

-- Q#03
--makeGame :: Secret -> Game
makeGameIfValid :: Either GameException Secret -> Either GameException Game
--makeGameIfValid (Left err) = Left err
--makeGameIfValid (Right s) = Right (makeGame s)
makeGameIfValid x = makeGame <$> x

-- Q#04
--toMaybe :: Bool -> a -> Maybe a
getDict :: IO (Maybe Dictionary)
{-
getDict =
    let io_exists = doesFileExist _DICT_FILE_
        m_exists = toMaybe <$> io_exists
    in
        m_exists <*> _DICT_
        -}
getDict = toMaybe <$> doesFileExist _DICT_FILE_ <*> _DICT_    
        