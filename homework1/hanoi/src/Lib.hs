-- | A library to do stuff.
module Lib
    (
      ourAdd
    , hanoi
    , hanoi4
    ) where

-- | Add two 'Int' values.
ourAdd :: Int  -- ^ left
       -> Int  -- ^ right
       -> Int  -- ^ sum
ourAdd x y = x + y

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b _       = [(a,b)]
hanoi 2 a b c       = [(a,c)] ++ (hanoi 1 a b c) ++ [(c,b)]
hanoi size a b c    = (hanoi (size-1) a b c) ++
                      (hanoi 1 a b c) ++
                      (hanoi (size-1) c b a)

-- I didn't understand the hanoi solution well enough to create
-- my own algorithm. Here is a solution I found online:
-- http://stackoverflow.com/questions/3607161/towers-of-hanoi-with-k-pegs/3615658
--
-- The 1, 2, 3 and 4 sized puzzles seemed to have the correct answers.
-- I was not able to get the minimum steps of 129 for a 15 size puzzle.
-- Further study of the Hanoi problem is required. See links:
-- https://notendur.hi.is/pgg/(ebook-pdf)%20-%20Mathematics%20-%20Concrete%20Mathematics.pdf
-- https://en.wikipedia.org/wiki/Tower_of_Hanoi#With_four_pegs_and_beyond
hanoi4  :: Integer  -- Size of puzzle
        -> Peg      -- Starting peg (source)
        -> Peg      -- Destination peg
        -> Peg      -- Temporary storage 1
        -> Peg      -- Temporary storage 2
        -> [Move]   -- Moves required to solve puzzle
hanoi4 1 source dest _ _     = [(source,dest)]
hanoi4 2 source dest tmp1 _  =
  [(source,tmp1), (source,dest), (tmp1,dest)]
hanoi4 size source dest tmp1 tmp2
  | size > 0  =
    (hanoi4 (size-2) source tmp1 tmp2 dest) ++
    [(source,tmp2), (source,dest), (tmp2, dest)] ++
    (hanoi4 (size-2) tmp1 dest source tmp2)
  | otherwise = []
