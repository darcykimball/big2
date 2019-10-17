#pragma once


#include <Python.h>


#include "comboindex.hpp"


namespace detail {


// Misc. useful constants.
static constexpr std::size_t NUM_HANDS_BY_SIZE[] = {
  combo::choose(52, 1),
  combo::choose(52, 2),
  combo::choose(52, 3),
  combo::choose(52, 4),
  combo::choose(52, 5)
};

static constexpr std::size_t DECK_SIZE = 52;


} // namespace detail


extern "C" {


PyObject* big2table_lookup_hand(PyListObject* cards);


} // extern "C"
