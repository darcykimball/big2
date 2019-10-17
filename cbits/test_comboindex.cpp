#include <iostream>


#include "comboindex.hpp"


int main() {
  //for (std::size_t i = 0; i < 5; i++) {
  //  for (std::size_t j = i + 1; j < 5; j++) {
  //    for (std::size_t k = j + 1; k < 5; k++) {
  //      std::cout << "i, j, k = " << i << ", " << j << ", " << k << '\n';
  //      auto index = combo::index<3>(5, { i, j, k });
  //      std::cout << " has index " << index << '\n';
  //    }
  //  }
  //}

  //for (std::size_t i = 0; i < 6; i++) {
  //  for (std::size_t j = i + 1; j < 6; j++) {
  //    for (std::size_t k = j + 1; k < 6; k++) {
  //      std::cout << "i, j, k = " << i << ", " << j << ", " << k << '\n';
  //      auto index = combo::index<3>(6, { i, j, k });
  //      std::cout << " has index " << index << '\n';
  //    }
  //  }
  //}

  for (std::size_t i = 0; i < 52; i++) {
    for (std::size_t j = i + 1; j < 52; j++) {
      for (std::size_t k = j + 1; k < 52; k++) {
        for (std::size_t l = k + 1; l < 52; l++) {
          for (std::size_t m = l + 1; m < 52; m++) {
            std::cout << "i, j, k, l, m = "
              << i << ", "
              << j << ", "
              << k << ", " 
              << l << ", " 
              << m << ", " << '\n';
            auto index = combo::index<5>(52, { i, j, k, l, m });
            std::cout << " has index " << index << '\n';
          }
        }
      }
    }
  }

  return 0;
}
