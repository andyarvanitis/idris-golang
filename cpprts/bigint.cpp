#include "bigint.h"

#if defined(SIMPLE_BIGINT)

namespace idris {

  using namespace std;

  ostream& operator << (ostream& stream, ubigint_t n) {
    if (n >= 10) {
      stream << n / 10;
    }
    return stream << static_cast<unsigned>(n % 10);
  }

  ostream& operator << (ostream& stream, bigint_t n) {
    if (n < 0) {
      stream << '-';
      n = -n;
    }
    return stream << static_cast<ubigint_t>(n);
  }

} // namespace idris

#endif // SIMPLE_BIGINT
