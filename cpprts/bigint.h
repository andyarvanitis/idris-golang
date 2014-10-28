#ifndef __idris_bigint_h_
#define __idris_bigint_h_

#include <cstdint>

//---------------------------------------------------------------------------------------
// If using GCC or clang on a 64bit system, use their built-in 128bit integer
// as a quick-and-dirty big int.
//---------------------------------------------------------------------------------------
#if defined(__GNUC__) && UINTPTR_MAX == 0xffffffffffffffff
#define SIMPLE_BIGINT
  #include <ostream>
  namespace idris {
    using bigint_t = __int128_t;
    using ubigint_t = __uint128_t;
    std::ostream& operator << (std::ostream&, bigint_t);
    std::ostream& operator << (std::ostream&, bigint_t);

    // Convert string literal to big int literal at compile time
    //
    #define asBig(n) asBig_entry(#n)
    constexpr bigint_t asBig_recurse(const char* str, const bigint_t num, const bool neg) {
      return (*str == '\0' ? (neg ? -num : num) 
                           : asBig_recurse(str+1, 10 * num + (*str - '0'), neg));
    }
    constexpr bigint_t asBig_entry(const char* str) {
      return asBig_recurse(*str == '-' ? str+1 : str, 0, *str == '-');
    }
  } // namespace idris

#else
  #warning "Inadequate big integer support detected! (using long long)"
  namespace idris {
    using bigint_t = long long;
    #define asBig(n) n
  } // namespace idris

#endif

#endif // __idris_bigint_h_
