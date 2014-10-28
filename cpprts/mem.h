#ifndef __idris_cpp_runtime_mem_h_
#define __idris_cpp_runtime_mem_h_

#include <cstring>

namespace idris {

inline void idris_memset(void* ptr, intptr_t offset, uint8_t c, intptr_t size) {
  memset(((uint8_t*)ptr) + offset, c, size);
}

inline uint8_t idris_peek(void* ptr, intptr_t offset) {
  return *(((uint8_t*)ptr) + offset);
}

inline void idris_poke(void* ptr, intptr_t offset, uint8_t data) {
  *(((uint8_t*)ptr) + offset) = data;
}

inline void idris_memmove(void* dest, void* src, intptr_t dest_offset, intptr_t src_offset, intptr_t size) {
  memmove((uint8_t*)dest + dest_offset, (uint8_t*)src + src_offset, size);
}

} // namespace idris

#endif // __idris_cpp_runtime_mem_h_
