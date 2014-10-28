#ifndef __idris_cpp_runtime_utility_h_
#define __idris_cpp_runtime_utility_h_

#include <algorithm>
#include "types.h"

namespace idris {

// Value charCode(const Value& value);
//
// Value fromCharCode(const Value& value);

char32_t utf8_head(const string&);

string utf8_tail(const string&);

char32_t char32_from_utf8_string(const string&, size_t index);

string systemInfo();

template <typename T>
inline T reverse(const T& container) {
  T rcontainer(container);
  std::reverse(rcontainer.begin(),rcontainer.end());
  return rcontainer;
}

string utf8_reverse(const string&);

} // namespace idris

#endif // __idris_cpp_runtime_utility_h_

