module Golf
  (
    skips
  , localMaxima
  , histogram
  , sumOccurrences
  , boolToHistogramPoint
  , histogramLevel
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

-- For the length of the integer list,
-- Look for occurences equal to thatamount
-- If it exists then print that line with those numbers occuring
histogram :: [Integer] -> String
histogram is
  | is == []  = footer
  | otherwise = (histogram' (toInteger maxOccurrences)) ++  footer
  where
    maxOccurrences = maximum (map (sumOccurrences is) [0..9])
    footer = "=========\n0123456789\n"
    histogram' :: Integer -> String
    histogram' 0 = ""
    histogram' x = (histogramLevel is x)  ++ (histogram' (x-1))

histogramLevel  :: [Integer]  -- List to work on
                -> Integer    -- Level of histogram to print
                -> String     -- The pinted out level
histogramLevel is level =  map boolToHistogramPoint (map (level<=) (map (sumOccurrences is) [0..9])) ++ "\n"

boolToHistogramPoint :: Bool -> Char
boolToHistogramPoint True  = '*'
boolToHistogramPoint False = ' '

sumOccurrences  :: [Integer]  -- List to find occurences
                -> Integer        -- Integer to find
                -> Integer        -- Number of occurences
sumOccurrences [] _         = 0
sumOccurrences (x:xs) comp
  | x == comp               = 1 + (sumOccurrences xs comp)
  | otherwise               = sumOccurrences xs comp
