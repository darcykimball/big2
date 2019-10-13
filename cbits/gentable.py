#!/usr/bin/env python3.7


import argparse
import itertools

from enum import Enum


"""
Generate a lookup table for hand types/ranks for Big 2. The legal hands are, in
order of increasing size/rank:

- 1
  - Single
- 2
  - Pair
- 3
  - Three of a kind
- 4
  - Two pair
  - Four of a kind
- 5
  - Straight
  - Full house

If two hands are the same type, then their lexicographical comparison gives the
higher hand. In addition to each of the legal hands listed, each size group
includes the 'illegal' type where the hand can't be classified as any other hand
type in its group. For example, the hand [2c 4h] is of size 2 and of 'illegal'
type.

The cards (0-51) are ordered in the following way:
0 -> 1c
1 -> 1d
2 -> 1h
3 -> 1s
4 -> 2c
5 -> 2d
...
51 -> Ks
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


def hands():
    """
    Generate all hands (of all sizes).
    """
    return itertools.chain(
        *[itertools.combinations(range(52), i + 1) for i in range(5)]
    )


def are_consecutive(xs):
    for i, x in enumerate(xs):
        if i == 0:
            continue
        if xs[i] != xs[i - 1] + 1:
            return False
    return True


def classify(hand):
    """
    Classify a hand (a list of `Card`s) to its type.

    Performs no validity checks on the cards themselves.
    """
    size = len(hand)
    ranks = sorted([card.rank() for card in hand])

    if size == 1:
        return HandType.SINGLE
    elif size == 2:
        if hand[0].rank() == hand[1].rank():
            return HandType.PAIR
    elif size == 3:
        # Check for trips
        if all([hand[i].rank() == hand[0].rank() for i in range(3)]):
            return HandType.TRIPS
    elif size == 4:
        # Check for quads
        if all([hand[i].rank() == hand[0].rank() for i in range(4)]):
            return HandType.QUADS

        # Check for two-pair
        group_sizes = [len(list(g)) for _, g in itertools.groupby(ranks)]
        if all([size == 2 for size in group_sizes]):
            return HandType.TWOPAIR
    elif size == 5:
        # Check for straight
        if are_consecutive(ranks) or ranks == [0, 9, 10, 11, 12]:  # Broadway
            return HandType.STRAIGHT

        # Check for full-house
        group_sizes = [len(list(g)) for _, g in itertools.groupby(ranks)]
        if group_sizes in [[2, 3], [3, 2]]:
            return HandType.FULLHOUSE

    return HandType.ILLEGAL


def dump_enum_decl():
    # XXX: Ghetto
    with open("handtype.h", "w") as f:
        f.write("typedef enum {\n")
        for type_ in HandType:
            f.write(f"  {type_.name},\n")
        f.write("} HandType;")


def dump_table(dump_hands=False):
    # XXX: Ghetto
    with open("handtypetable.c", "w") as f:
        f.write('#include "handtype.h"\n\n')

        f.write("HandType hand_type_table[] = {\n")
        for h in hands():
            cards = list(map(Card, h))
            hand_type = classify(cards)
            f.write(f"  {hand_type.name},")
            if dump_hands:
                f.write(f" // {cards}")
            f.write("\n")
        f.write("};")
        

def main():
    parser = argparse.ArgumentParser(
        description="Generate a C lookup table for Big 2"
    )
    parser.add_argument("--dump-hands", action="store_true",
                        help="Dump hand representations with the table")
    args = parser.parse_args()

    dump_enum_decl()
    dump_table(args.dump_hands)


if __name__ == "__main__":
    main()
