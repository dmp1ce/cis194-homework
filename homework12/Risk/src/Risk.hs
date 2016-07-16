{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List
import Data.Monoid

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army } deriving (Show)

battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield a d)  = do
  -- Get rolls for attacker
  a_rolls <- dieRolls $ min (a-1) 3
  -- Get rolls for defender
  d_rolls <- dieRolls $ min d 2
  -- Determine losses by attacker and defender
  let (Sum a_losses, Sum d_losses) = losses a_rolls d_rolls
  return $ Battlefield (a - a_losses) (d - d_losses)

invade :: Battlefield -> Rand StdGen Battlefield
invade (Battlefield a d)
  | a < 2 || d < 1  = return $ Battlefield a d
  | otherwise       = battle (Battlefield a d) >>= invade

-- A list of rolls
dieRolls :: Int -> Rand StdGen [DieValue]
dieRolls i = flipType $ take i $ repeat die

--            (Attacker, Defender)
type Losses = (Sum Army, Sum Army)

-- Determine losses
--        attacker    -> defender   -> (attacker losses, defender losses)
losses :: [DieValue]  -> [DieValue] -> Losses
losses a d = losses' (sort a) (sort d) where
  losses' ((DV aR):as) ((DV dR):ds)
    | aR > dR   = (Sum 0,Sum 1) <> losses' as ds
    | otherwise = (Sum 1,Sum 0) <> losses' as ds
  losses' [] _  = mempty
  losses' _ []  = mempty

-- From the Haskell Programming Monad chappter exercises
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _      = return []
meh (a:as) f  = (:) <$> (f a >>= (\x -> return $ x)) <*> (meh as f)

flipType :: (Monad m) => [m a] -> m [a]
flipType mas = meh mas (\ma -> ma >>= (\a -> return a))
