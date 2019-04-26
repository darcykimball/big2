{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Game.BigTwo.Card where


import Game.Implement.Card
import Game.Implement.Card.Standard (PlayingCard(..), Rank(..), Suit(..))
import Game.Implement.Card.Standard.Poker (Order(..))


-- This is a newtype over PlayingCard so we can use different class instances
newtype BTCard = BTCard { getBTCard :: PlayingCard }
  deriving (Read, Show, Eq, Enum, Ord, Bounded)


instance Card BTCard


-- XXX: Dammit.
instance ValuedCard BTCard Rank where
  toValue = toValue . getBTCard


instance ValuedCard BTCard Suit where
  toValue = toValue . getBTCard


toSuit :: BTCard -> Suit
toSuit = toValue


toRank :: BTCard -> Rank
toRank = toValue


-- Order type for use in OrderedCard instance 
data BigTwoOrder = BigTwoOrder
  deriving (Show, Eq, Ord)


--
-- Instances
--


instance OrderedCard BTCard BigTwoOrder where
  compareCardBy _ 
    (BTCard c1@(PlayingCard r1 s1)) (BTCard c2@(PlayingCard r2 s2))
    | r1 == r2  = compare s1 s2
    | r1 == Two = GT
    | r2 == Two = LT
    | otherwise =
        case rankCmp of
          EQ -> compareCardBy SuitOrder c1 c2
          result -> result
      where
        rankCmp = compareCardBy AceHighRankOrder c1 c2
