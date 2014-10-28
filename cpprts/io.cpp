
#include <iostream>
#include <fstream>
#include <cassert>
#include "io.h"
#include "exceptions.h"

//---------------------------------------------------------------------------------------
// C++ versions of the same C runtime functions
//---------------------------------------------------------------------------------------

namespace idris {

nullptr_t putStr(const string str) {
  cout << str;
  return nullptr;
}

shared_ptr<void> fileOpen(const string name, const string mode) {
  auto openmode = fstream::openmode(0x00);
  for (auto flag : mode) {
    switch (flag) {
      case 'r':
        openmode |= fstream::in;
        break;
      case 'w':
        openmode |= fstream::out | fstream::trunc;
        break;
      case 'a':
        openmode |= fstream::out | fstream::app;
        break;
      case 'b':
        openmode |= fstream::binary;
        break;
      case '+':
        openmode |= fstream::out | (openmode & fstream::in ? fstream::app : fstream::in);
        break;
    }
  }
  auto file = make_shared<fstream>();
  file->open(name, openmode);
  return file->fail() ? nullptr : file;
}

nullptr_t fileClose(shared_ptr<void> h) {
  assert(h);
  auto file = static_pointer_cast<fstream>(h);
  file->close();
  return nullptr;
}

string freadStr(shared_ptr<void> h) {
  assert(h);
  auto file = static_pointer_cast<fstream>(h);
  string str;
  getline(*file,str);
  if (not file->eof()) {
    str += '\n';
  }
  return str;
}

nullptr_t fputStr(shared_ptr<void> h, const string str) {
  assert(h);
  auto file = static_pointer_cast<fstream>(h);
  *file << str;
  return nullptr;
}

int fileEOF(shared_ptr<void> h) {
  assert(h);
  auto file = static_pointer_cast<fstream>(h);
  return file->eof();
}

int fileError(shared_ptr<void> h) {
  assert(h);
  auto file = static_pointer_cast<fstream>(h);
  return file->fail();
}

} // namespace idris

