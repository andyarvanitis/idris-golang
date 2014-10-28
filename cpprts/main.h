#ifndef __idris_cpp_runtime_main_h_
#define __idris_cpp_runtime_main_h_

namespace idris {

struct IdrisMain {
  static int argc;
  static char ** argv;
};

inline int idris_numArgs() {
  return IdrisMain::argc;
}

inline const char* idris_getArg(int i) {
  return IdrisMain::argv[i];
}

} // namespace idris

#endif // __idris_cpp_runtime_main_h_
