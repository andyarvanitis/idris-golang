#include <sstream>
#include <iostream>
#include <deque>

#if defined(__clang__)
  #if __has_include(<codecvt>)
    #include <codecvt>
    #include <locale>
    #define CODECVT_AVAILABLE
  #endif
#endif

#include "box.h"
#include "types_aliases.h"

namespace idris {
  
using namespace std;

template <typename T>
string interpreted_string(T value) {
  ostringstream strstream;
  strstream << value;
  return strstream.str();
}

void cannot_convert(char src, string tgt) {
  cout << "Cannot convert type " << src << " to " << tgt << "!" << endl;
}

//---------------------------------------------------------------------------------------
// Int
//---------------------------------------------------------------------------------------
template struct TypedBoxedValue<'i', int>;

template <>
string TypedBoxedValue<'i', int>::asString() const {
  return to_string(value);
}

template <>
long long int TypedBoxedValue<'i', int>::asIntegral() const {
  return value;
}

//---------------------------------------------------------------------------------------
// BigInt
//---------------------------------------------------------------------------------------
template struct TypedBoxedValue<'b', bigint_t>;

template <>
string TypedBoxedValue<'b', bigint_t>::asString() const {
  return interpreted_string(value);
}

template <>
long long int TypedBoxedValue<'b', bigint_t>::asIntegral() const {
  return value;
}

//---------------------------------------------------------------------------------------
// Float
//---------------------------------------------------------------------------------------
template struct TypedBoxedValue<'f', double>;

template <>
string TypedBoxedValue<'f', double>::asString() const {
  return to_string(value);
}

template <>
long long int TypedBoxedValue<'f', double>::asIntegral() const {
  return value;
}

//---------------------------------------------------------------------------------------
// String
//---------------------------------------------------------------------------------------
template struct TypedBoxedValue<'s', string>;

template <>
string TypedBoxedValue<'s', string>::asString() const {
  return value;
}

template <>
long long int TypedBoxedValue<'s', string>::asIntegral() const {
  cannot_convert(this->getTypeId(), "integer");
  return 0;
}

//---------------------------------------------------------------------------------------
// Char
//---------------------------------------------------------------------------------------
template struct TypedBoxedValue<'c', char32_t>;

template <>
string TypedBoxedValue<'c', char32_t>::asString() const {
  #if defined CODECVT_AVAILABLE  
    std::wstring_convert<std::codecvt_utf8<char32_t>, char32_t> utf32conv;
    string utf8 = utf32conv.to_bytes(value);  
  #else  
    string utf8 = string(1, static_cast<char>(value)); // no unicode support, for now
  #endif
  return utf8;
}

template <>
long long int TypedBoxedValue<'c', char32_t>::asIntegral() const {
  return value;
}

//---------------------------------------------------------------------------------------
// Word8
//---------------------------------------------------------------------------------------
template struct TypedBoxedValue<'1', uint8_t>;

template <>
string TypedBoxedValue<'1', uint8_t>::asString() const {
  return to_string(value);
}

template <>
long long int TypedBoxedValue<'1', uint8_t>::asIntegral() const {
  return value;
}

//---------------------------------------------------------------------------------------
// Word16
//---------------------------------------------------------------------------------------
template struct TypedBoxedValue<'2', uint16_t>;

template <>
string TypedBoxedValue<'2', uint16_t>::asString() const {
  return to_string(value);
}

template <>
long long int TypedBoxedValue<'2', uint16_t>::asIntegral() const {
  return value;
}

//---------------------------------------------------------------------------------------
// Word32
//---------------------------------------------------------------------------------------
template struct TypedBoxedValue<'4', uint32_t>;

template <>
string TypedBoxedValue<'4', uint32_t>::asString() const {
  return to_string(value);
}

template <>
long long int TypedBoxedValue<'4', uint32_t>::asIntegral() const {
  return value;
}

//---------------------------------------------------------------------------------------
// Word64
//---------------------------------------------------------------------------------------
template struct TypedBoxedValue<'8', uint64_t>;

template <>
string TypedBoxedValue<'8', uint64_t>::asString() const {
  return to_string(value);
}

template <>
long long int TypedBoxedValue<'8', uint64_t>::asIntegral() const {
  return value;
}

//---------------------------------------------------------------------------------------
// ManagedPtr
//---------------------------------------------------------------------------------------
template struct TypedBoxedValue<'m', shared_ptr<void>>;

template <>
string TypedBoxedValue<'m', shared_ptr<void>>::asString() const {
  return value ? interpreted_string(value.get()) : "nullptr";
}

template <>
long long int TypedBoxedValue<'m', shared_ptr<void>>::asIntegral() const {
  cannot_convert(this->getTypeId(), "integer");
  return 0;
}

//---------------------------------------------------------------------------------------
// Ptr
//---------------------------------------------------------------------------------------
template struct TypedBoxedValue<'p', void*>;

template <>
string TypedBoxedValue<'p', void*>::asString() const {
  return interpreted_string(value);
}

template <>
long long int TypedBoxedValue<'p', void*>::asIntegral() const {
  cannot_convert(this->getTypeId(), "integer");
  return 0;
}

//---------------------------------------------------------------------------------------
// Con
//---------------------------------------------------------------------------------------
template struct TypedBoxedValue<'C', Constructor>;

template <>
string TypedBoxedValue<'C', Constructor>::asString() const {
  cannot_convert(this->getTypeId(), "string");
  return "";
}

template <>
long long int TypedBoxedValue<'C', Constructor>::asIntegral() const {
  cannot_convert(this->getTypeId(), "integer");
  return 0;
}

Constructor::~Constructor() {
  // Unroll recursive Constructor destructions to prevent blowing the stack
  Args _args_ = args;
  args.clear(); // "ownership" taken by _args_ local container

  deque<Value> cons;
  bool done = false;

  while (not done) {
    for (const Value& arg : _args_) {
      if (arg and arg->getTypeId() == 'C' and arg.unique()) {
        cons.push_back(arg);
      }
    }

    _args_.clear();

    if (cons.empty()) {
      done = true;
    } else {
      _args_ = unbox<Con>(cons.front()).args;
      cons.pop_front();
    }
  }
}

} // namespace idris
