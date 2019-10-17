#include <cstddef> // FIXME: NULL or nullptr?


#include "big2table.hpp"
#include "comboindex.hpp"
#include "handtype.h"
#include "debug.hpp" // FIXME: remove!


// XXX: Unsafe; assumes list has been validated.
template <std::size_t K>
static auto to_array(PyObject* cards) {
  combo::combo_repr<K> combo;

  for (std::size_t i = 0; i < K; ++i) {
    auto pyvalue = PyList_GET_ITEM(cards, i);
    auto value = PyLong_AsSize_t(pyvalue);
    combo[i] = value;
  }

  return combo;
}


PyObject* big2table_lookup_hand(PyObject* /* self */, PyObject* args) {
  std::cerr << "Hello.\n";

  PyObject* cards = NULL; // The sole argument: a list of cards

  // FIXME: Make safer using 'O!' or otherwise.
  if (!PyArg_ParseTuple(args, "O", &cards))
    return NULL;

  // Some sanity checks.
  std::size_t len = PyList_Size(cards);
  if (len > 5 or len == 0) {
    // TODO/FIXME: raise ValueError?
  }

  // TODO: Also check validity of cards

  std::cerr << "Huh.\n";

  // Get the hand's index.
  std::size_t index;
  switch (len) {
    case 1:
      index = combo::index(detail::DECK_SIZE, to_array<1>(cards));
      break;
    case 2:
      index = combo::index(detail::DECK_SIZE, to_array<2>(cards));
      break;
    case 3:
      index = combo::index(detail::DECK_SIZE, to_array<3>(cards));
      break;
    case 4:
      index = combo::index(detail::DECK_SIZE, to_array<4>(cards));
      break;
    case 5:
      index = combo::index(detail::DECK_SIZE, to_array<5>(cards));
      break;
    default:
      // FIXME/TODO: The impossible happened. Raise?
      return NULL;
  }

  std::cerr << "Uh.\n";

  // Offset the index according to hand size.
  for (std::size_t i = 0; i < len - 1; ++i) {
    index += detail::NUM_HANDS_BY_SIZE[i];
  }

  auto hand_value = hand_type_table[index];

  std::cerr << "What?\n";

  // FIXME: Really, this should be a discriminated union that's returned, but
  // it seems like it'd be clunky to define the Python (enum) type in C. For
  // now, let's leave the glue code to Pythonland.
  return PyLong_FromSize_t(hand_value);
}


//
// Module stuff.
//


static PyMethodDef big2table_methods[] = {
  {
    "lookup_hand",
    big2table_lookup_hand,
    METH_VARARGS,
    "Look up a hand's value."
  },
  { NULL, NULL, 0,  NULL } 
};


static PyModuleDef big2table_module {
  PyModuleDef_HEAD_INIT,
  .m_name = "big2table",
  .m_doc = "Module for looking up Big2 hand ranks.",
  .m_size = -1,
  .m_methods = big2table_methods,
};


PyMODINIT_FUNC PyInit_big2table() {
  return PyModule_Create(&big2table_module);
}
