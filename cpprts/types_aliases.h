#ifndef __idris_cpp_types_aliases_h_
#define __idris_cpp_types_aliases_h_

#include "box.h"
#include "bigint.h"

namespace idris {

using namespace std;

using Int        = TypedBoxedValue<'i', int>;
using BigInt     = TypedBoxedValue<'b', bigint_t>;
using Float      = TypedBoxedValue<'f', double>;
using String     = TypedBoxedValue<'s', string>;
using Char       = TypedBoxedValue<'c', char32_t>;
using Word8      = TypedBoxedValue<'1', uint8_t>;
using Word16     = TypedBoxedValue<'2', uint16_t>;
using Word32     = TypedBoxedValue<'4', uint32_t>;
using Word64     = TypedBoxedValue<'8', uint64_t>;
using ManagedPtr = TypedBoxedValue<'m', shared_ptr<void>>;
using Ptr        = TypedBoxedValue<'p', void*>;
using Con        = TypedBoxedValue<'C', Constructor>;

} // namespace idris

#endif // __idris_cpp_types_aliases_h_
