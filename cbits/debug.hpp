#pragma once


#include <iostream>


namespace debug {


// Print a container of...printables.
auto print_many = [](auto&& xs) {
  for (auto const& x : xs) {
    std::cerr << x << ' ';
  }
  std::cerr << '\n';
};


} // namespace debug
