-- A rotating (possibly empty) list zipper
module Util.RotatingZipper where


data RZipper a = RZipper [a] (Maybe a) [a]
  deriving (Show, Eq, Ord)

