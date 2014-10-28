#ifndef __idris_cpp_types_h_
#define __idris_cpp_types_h_

#include "box.h"
#include "types_aliases.h"
#include "types_extern.h"

namespace idris {

using namespace std;  

//---------------------------------------------------------------------------------------
// Special handling for C-style string conversions
//---------------------------------------------------------------------------------------

template <>
inline auto box<String, const char*>(const char * && s) -> Value {
  return s ? static_pointer_cast<BoxedValue>(make_shared<String>(string(s))) : nullptr;
}

template <>
inline auto box<String, char*>(char * && s) -> Value {
  return s ? static_pointer_cast<BoxedValue>(make_shared<String>(string(s))) : nullptr;
}

//-------------------------------------------------------------------------------------------------
// Best-fit conversions from native C++ types to runtime types
//-------------------------------------------------------------------------------------------------

template <typename T>
struct FromNative {
};

template <>
struct FromNative<int> {
  using type = Int;
};

template <>
struct FromNative<bigint_t> {
  using type = BigInt;
};

template <>
struct FromNative<long long> {
  using type = BigInt;
};

template <>
struct FromNative<double> {
  using type = Float;
};

template <>
struct FromNative<string> {
  using type = String;
};

template <>
struct FromNative<const char*> {
  using type = String;
};

template <>
struct FromNative<char32_t> {
  using type = Char;
};

template <>
struct FromNative<uint16_t> {
  using type = Word16;
};

template <>
struct FromNative<uint32_t> {
  using type = Word32;
};

template <>
struct FromNative<uint64_t> {
  using type = Word64;
};

template <>
struct FromNative<void*> {
  using type = Ptr;
};

} // namespace idris


#endif // __idris_cpp_types_h_

