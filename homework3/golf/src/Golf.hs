module Golf
  (
    skips
  , localMaxima
  ) where

-- I realized that splitAt in Prelude was close to what I needed so
-- I modeled my skipAt function after splitAt. Skip uses skipAt over
-- a list of integers numbering 0 to the size of the original list.
skips :: [a] -> [[a]]
skips []  = []
skips x   = map (skipAt x) [0..((length x) -1)] 
  where
    skipAt :: [a] -> Int -> [a]
    skipAt ls n
      | n <= 0    = ls
      | otherwise = skipAt' n ls
      where
        skipAt' :: Int -> [a] -> [a]
        skipAt' _ []        = []
        skipAt' 0 (y:ys)
          | length ys > n   = [y] ++ (skipAt' n ys)
          | length ys <= n  = [y]
        skipAt' m (_:ys)    = skipAt' (m-1) ys

-- The function has to look at at least 3 integers. If the 3 integers
-- are not available then just return an empty list.
localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:ss)
  | x < y && z < y  = [y] ++ localMaxima (y:z:ss)
  | otherwise       = localMaxima (y:z:ss)
localMaxima _       = []