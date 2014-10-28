#ifndef __idris_cpp_runtime_exceptions_h_
#define __idris_cpp_runtime_exceptions_h_

#include <sstream>
#include <iostream>

#if defined(__clang__)
  #if !__has_feature(cxx_exceptions)
    #define IDRIS_RUNTIME_NO_EXECPTIONS
  #endif
#endif

#if defined(IDRIS_RUNTIME_NO_EXECPTIONS)
  #include <cassert>
  #define RAISE(msg,value) std::cout << "Idris C++ runtime exception: " << msg << value << std::endl; assert(false);
#else
  #define RAISE(msg,value) {\
  struct idris_cpp_runtime : std::exception { \
    std::ostringstream message; \
    const char* what() const noexcept { \
      return message.str().c_str(); \
    } \
  }; \
  idris_cpp_runtime exception; \
  exception.message << msg << value; \
  throw exception; \
  }
#endif
  
#endif // __idris_cpp_runtime_exceptions_h_
