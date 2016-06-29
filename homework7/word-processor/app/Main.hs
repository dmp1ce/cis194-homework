module Main where

import JoinListBuffer
import JoinList (jlFromString)
import Editor

main = runEditor editor $ jlFromString $ unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]
