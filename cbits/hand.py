#!/usr/bin/env python3.7


from enum import Enum


"""
Representation of cards and hands in Big2.
"""


class Card():
    """
    Thin convenience wrapper for our specific card representation.
    """

    SUITS = ["c", "d", "h", "s"]
    RANKS = ["A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K"]

    def __init__(self, value):
        self.value = value

    def rank(self):
        return self.value // 4

    def suit(self):
        return self.value % 4

    def __str__(self):
        return self.RANKS[self.rank()] + self.SUITS[self.suit()]

    def __repr__(self):
        return str(self)


class HandType(Enum):
    """
    Closed enumeration of hand types.
    """
    ILLEGAL = 0
    SINGLE = 1
    PAIR = 2
    TWOPAIR = 3
    TRIPS = 4
    STRAIGHT = 5
    FULLHOUSE = 6
    QUADS = 7
