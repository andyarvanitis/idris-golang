#ifndef __idris_cpp_types_extern_h_
#define __idris_cpp_types_extern_h_

#include "box.h"

namespace idris {

using namespace std;

extern template struct TypedBoxedValue<'i', int>;
extern template struct TypedBoxedValue<'b', bigint_t>;
extern template struct TypedBoxedValue<'f', double>;
extern template struct TypedBoxedValue<'s', string>;
extern template struct TypedBoxedValue<'c', char32_t>;
extern template struct TypedBoxedValue<'1', uint8_t>;
extern template struct TypedBoxedValue<'2', uint16_t>;
extern template struct TypedBoxedValue<'4', uint32_t>;
extern template struct TypedBoxedValue<'8', uint64_t>;
extern template struct TypedBoxedValue<'m', shared_ptr<void>>;
extern template struct TypedBoxedValue<'p', void*>;
extern template struct TypedBoxedValue<'C', Constructor>;

} // namespace idris


#endif // __idris_cpp_types_extern_h_
