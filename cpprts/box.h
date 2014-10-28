#ifndef __idris_cpp_box_h_
#define __idris_cpp_box_h_

#include <memory>
#include <vector>
#include <string>
#include <cassert>

namespace idris {

using namespace std;  

//---------------------------------------------------------------------------------------

struct BoxedValue {  
  virtual char getTypeId() const = 0;
  virtual string asString() const = 0;
  virtual long long int asIntegral() const = 0;
  virtual ~BoxedValue() {}
};

//---------------------------------------------------------------------------------------

template <char N, typename T>
struct TypedBoxedValue : public BoxedValue {
  
  using type = T;
  
  char getTypeId() const { return N; }
  
  const T value;
  
  template <typename... ArgTypes>
  TypedBoxedValue(ArgTypes&&... args) : value(forward<ArgTypes>(args)...) {}
  
  string asString() const;
  long long int asIntegral() const;
};

//---------------------------------------------------------------------------------------

using Value = shared_ptr<BoxedValue>;

//---------------------------------------------------------------------------------------
// Boxing
//---------------------------------------------------------------------------------------

template <typename T, typename... ArgTypes>
inline auto box(ArgTypes&&... args) -> Value {
  return static_pointer_cast<BoxedValue>(make_shared<T>(forward<ArgTypes>(args)...));
}

//---------------------------------------------------------------------------------------
// Unboxing
//---------------------------------------------------------------------------------------

template <typename T>
inline auto unbox(const Value& boxedValue) -> const decltype(T::value) & {
  assert(boxedValue);
  return dynamic_pointer_cast<T>(boxedValue)->value;
}

//---------------------------------------------------------------------------------------
// Special constructor
//---------------------------------------------------------------------------------------

struct VirtualMachine;

using IndexType = size_t;

struct Constructor {
  
  using Args = vector<Value>;
  
  const size_t tag;
  Args args; // non-const to allow unrolled destruction

  template <typename ... ArgTypes>
  Constructor(const size_t tag, ArgTypes&&... args)
    : tag(tag)
    , args({args...})
    {}

  Constructor(const Constructor&) = delete;

  ~Constructor();
};

} // namespace idris


#endif // __idris_cpp_box_h_

