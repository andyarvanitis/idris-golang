#ifndef __idris_cpp_runtime_io_h_
#define __idris_cpp_runtime_io_h_

#include <memory>
#include <string>

namespace idris {
  
using namespace std;  

nullptr_t putStr(const string str);

shared_ptr<void> fileOpen(const string name, const string mode);

nullptr_t fileClose(shared_ptr<void> h);

string freadStr(shared_ptr<void> h);

nullptr_t fputStr(shared_ptr<void> h, const string str);

int fileEOF(shared_ptr<void> h);

int fileError(shared_ptr<void> h);

} // namespace idris

#endif // __idris_cpp_runtime_io_h_

