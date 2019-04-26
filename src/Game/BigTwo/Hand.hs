{-# LANGUAGE RecordWildCards #-}
module Game.BigTwo.Hand where


import Data.Function (on)
import Data.List (group, sortBy, sort)
import Game.Implement.Card
import Game.Implement.Card.Standard hiding (toSuit, toRank)


import Game.BigTwo.Card


import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M


--
-- Allowed hands (to play) and some utilities
--


data HandType =
    Single
  | Pair
  -- No two-pair
  | Triple
  | Straight
  | Flush
  | FullHouse
  | Quads
  | StraightFlush
  deriving (Show, Eq, Ord)


toMaybe :: Bool -> a -> Maybe a
toMaybe True a = Just a
toMaybe _    _ = Nothing


allSame :: Eq a => [a] -> Bool
allSame (x:xs) = all (== x) xs
allSame _      = True


checkHandType :: [BTCard] -> Maybe HandType
checkHandType cards =
  case cards of
    [] -> Nothing
    [a] -> Just Single
    cs@[a,b] -> toMaybe (allSame $ map toRank cs) Pair
    cs@[a,b,c] -> toMaybe (allSame $ map toRank cs) Triple
    cs@[a,b,c,d] -> toMaybe (allSame $ map toRank cs) Quads
    cs@[a,b,c,d,e]
      | flush -> Just Flush
      | straight -> Just Straight
      | flush && straight -> Just StraightFlush
      | fullHouse -> Just FullHouse
      | otherwise -> Nothing
      where
        flush = isFlush cs
        straight = isStraight cs
        fullHouse = isFullHouse cs


isFullHouse :: [BTCard] -> Bool
isFullHouse = hasRightShape . group . sort . map toRank 
  where
    hasRightShape [[a,b,c],[d,e]] = True
    hasRightShape [[a,b],[c,d,e]] = True
    hasRightShape _               = False


isFlush :: [BTCard] -> Bool
isFlush = allSame . map toSuit


-- Check if a hand is a straight. This accounts for 'wraparound' straights, e.g.
-- KQA23.
isStraight :: [BTCard] -> Bool
isStraight [] = True
isStraight cs = go vals $ head vals
  where
    sorted = sortBy (compareCardBy BigTwoOrder) cs
    vals = map toRank sorted
    go (x:xs) y = x == y && go xs (succValueCycle y)
    go _      _ = True


-- Cyclic value relation, i.e. 3 -> 4 -> ... -> A -> 2 -> 3 -> ...; relies on
-- Rank's derived Enum instance.
succValueCycle :: Rank -> Rank
succValueCycle King = Ace
succValueCycle r    = succ r
