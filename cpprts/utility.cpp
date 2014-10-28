#include "utility.h"

#include <sstream>
#include <iostream>
#include <sys/utsname.h>

#if defined(__clang__)
  #if __has_include(<codecvt>)
    #include <codecvt>
    #include <locale>
    #define CODECVT_AVAILABLE
  #endif
#endif

namespace idris {

using namespace std;

#if defined(CODECVT_AVAILABLE)

char32_t utf8_head(const string& str) {
  static wstring_convert<codecvt_utf8<char32_t>, char32_t> utf32conv;
  auto utf32 = utf32conv.from_bytes(str);
  return utf32.front();
}

string utf8_tail(const string& str) {
  static wstring_convert<codecvt_utf8<char32_t>, char32_t> utf32conv;
  auto utf32 = utf32conv.from_bytes(str);
  auto utf32_tail = utf32.substr(1, utf32.length() - 1);
  return utf32conv.to_bytes(utf32_tail);
}

char32_t char32_from_utf8_string(const string& str, size_t index) {
  static wstring_convert<codecvt_utf8<char32_t>, char32_t> utf32conv;
  auto utf32 = utf32conv.from_bytes(str);
  return utf32.at(index);
}

string utf8_reverse(const string& str) {
  static wstring_convert<codecvt_utf8<char32_t>, char32_t> utf32conv;
  auto utf32 = utf32conv.from_bytes(str);
  auto utf32_reversed = reverse(utf32);
  return utf32conv.to_bytes(utf32_reversed);  
}

#else // no codecvt available --  no unicode support, for now

char32_t utf8_head(const string& str) {
  return str.front();
}

string utf8_tail(const string& str) {
  return str.substr(1, str.length() - 1);
}

char32_t char32_from_utf8_string(const string& str, size_t index) {
  return str.at(index);
}

string utf8_reverse(const string& str) {
  return reverse(str);
}

#endif // CODECVT_AVAILABLE

string systemInfo() {
  ostringstream infoStr;
  infoStr << "C++11 backend";
  utsname info;
  if (uname(&info) > -1) {
    infoStr << " on " << info.sysname;
    infoStr << " "    << info.machine;
  }
  return infoStr.str();
}

} // namespace idris
