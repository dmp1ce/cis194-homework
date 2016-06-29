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
  )

instance Buffer (JoinList (Score, Size) String) where
  toString     = jlToString
  fromString   = jlFromString
  line n b     = indexJ n b
  replaceLine n l b = jlReplaceLine n l b
--unlines . uncurry replaceLine' . splitAt n . lines $ b
--      where replaceLine' pre [] = pre
--            replaceLine' pre (_:ls) = pre ++ l:ls
  numLines     = jlSize
  value _      = (0::Int)
