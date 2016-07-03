module Main where

import Data.Tree
import Employee
import Party

main :: IO ()
main = do
  s <- readFile "data/company.txt"
  let empTree = (read s :: Tree Employee)
  putStrLn $ outputGuestList $ maxFun empTree
