{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module JoinListBuffer where

import Buffer
import Sized
import Scrabble
import JoinList
  ( jlToString
  , jlFromString
  , indexJ
  , JoinList
  , jlReplaceLine
  , jlSize
  , jlScore
  )

instance Buffer (JoinList (Score, Size) String) where
  toString          = jlToString
  fromString        = jlFromString
  line n b          = indexJ n b
  replaceLine n l b = jlReplaceLine n l b
  numLines          = jlSize
  value             = jlScore
