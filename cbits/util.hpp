#pragma once


#include <utility>


namespace util {


// XXX: Sigh.
template <typename Wrapper, typename ...>
struct integer_sequence_tail; 

template <typename T, T I, T ...Is>
struct integer_sequence_tail<std::integer_sequence<T, I, Is...>> {
  using type = typename std::integer_sequence<T, Is...>;
};


} // namespace util
