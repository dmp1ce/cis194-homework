{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Scrabble
  ( Score (Score)
  , score
  , getScore
  , scoreString
  )
  where
import Data.Char

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

score :: Char -> Score
score c
  | (toLower c) `elem` ['a','e','i','l','n','o','r','s','t','u']  = Score 1
  | (toLower c) `elem` ['d','g']              = Score 2
  | (toLower c) `elem` ['b','c','m','p']      = Score 3
  | (toLower c) `elem` ['f','h','v','w','y']  = Score 4
  | (toLower c) `elem` ['k']      = Score 5
  | (toLower c) `elem` ['j','x']  = Score 8
  | (toLower c) `elem` ['q','z']  = Score 10
  | otherwise                     = Score 0

scoreString :: String -> Score
scoreString s = foldr (\c acc -> (score c) + acc) mempty s

instance Monoid Score where
  mempty  = Score 0
  mappend = (+)

getScore :: Score -> Int
getScore (Score i) = i
