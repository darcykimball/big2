#pragma once


#include <algorithm>
#include <array>
#include <cstdint>


#include "debug.hpp" // FIXME: remove!
#include "util.hpp"


namespace combo {


template <std::size_t K>
using combo_repr = std::array<std::size_t, K>;


// XXX: From https://stackoverflow.com/questions/9330915/number-of-combinations-n-choose-r-in-c
constexpr std::size_t choose(std::size_t n, std::size_t k) {
  if (k > n) return 0;
  if (k * 2 > n) k = n - k;
  if (k == 0) return 1;

  std::size_t result = n;
  for(std::size_t i = 2; i <= k; ++i) {
      result *= (n - i + 1);
      result /= i;
  }
  return result;
}


// Get the index of a combination, assuming that they're ordered odometer-style.
template <
  std::size_t K,
  typename Indices = std::make_index_sequence<K>
>
std::size_t index(std::size_t N, combo_repr<K> const& combo);


template <std::size_t K, std::size_t ...Is>
std::size_t index_impl(
  std::size_t N,
  combo_repr<K> const& combo,
  std::integer_sequence<std::size_t, Is...>
) {
  // FIXME: remove.
  //std::cerr << "index_impl<" << N << ", " << K << ">(\n  ";
  //debug::print_many(combo);
  //std::cerr << ")\n";

  if constexpr (K == 1) {
    return combo[0];
  } else {
    // Examine the leftmost (most 'significant') digit to get its contributed
    // offset.
    auto leftmost = combo[0];
    std::size_t offset{0};
    for (std::size_t i = 0; i < leftmost; ++i) {
      offset += choose(N - i - 1, K - 1);
    } 

    // FIXME: remove
    //std::cerr << "Offset = " << offset << '\n';

    // Create a shifted (sub)combination from the other values
    combo_repr<K - 1> shifted{ (combo[Is] - leftmost - 1)... };
    auto subindex = index<K - 1>(N - leftmost - 1, shifted);

    return offset + subindex;
  }
}


template <
  std::size_t K,
  typename Indices = std::make_index_sequence<K>
>
std::size_t index(std::size_t N, combo_repr<K> const& combo) {
  using subcombo_indices = typename util::integer_sequence_tail<Indices>::type;
  return index_impl<K>(N, combo, subcombo_indices{});
}


} // namespace combo
