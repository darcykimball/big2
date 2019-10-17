#!/usr/bin/env python3


import big2table
import gentable


def test_all_hands():
    for hand in gentable.hands():
        hand_rank = big2table.lookup_hand(list(hand))
        print(f"hand = {hand}, rank = {hand_rank}")


if __name__ == "__main__":
    test_all_hands()
