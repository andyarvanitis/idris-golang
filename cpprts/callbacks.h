#ifndef __idris_cpp_runtime_callbacks_h_
#define __idris_cpp_runtime_callbacks_h_

#include "types.h"
#include "vm.h"

namespace idris {

//-------------------------------------------------------------------------------------------------

void process_args(shared_ptr<VirtualMachine>&, Value&) {
}

template <typename A, typename... ArgTypes>
void process_args(shared_ptr<VirtualMachine>& vm, Value& res, A arg, ArgTypes&&... args) {

  void _idris__123_APPLY0_125_(shared_ptr<VirtualMachine>&, IndexType);

  if (res->getTypeId() == Con::typeId) {
    reserve(vm, vm->valstack_top + 2);
    vm->valstack[vm->valstack_top] = res;
    vm->valstack[vm->valstack_top + 1] = box<typename FromNative<A>::type>(arg);
    auto myoldbase = vm->valstack_base;
    vm->valstack_base = vm->valstack_top;
    vm->valstack_top += 2;
    vm_call(vm, _idris__123_APPLY0_125_, myoldbase);
    res = vm->ret;
  }

  process_args(vm, res, forward<ArgTypes>(args)...);
}

//-------------------------------------------------------------------------------------------------

template <typename RetType, typename... ArgTypes>
RetType proxy_function(const weak_ptr<VirtualMachine>& vm_weak, 
                       const weak_ptr<BoxedValue>& con_weak, 
                       ArgTypes... args) {  

  // Make sure the vm hasn't been destroyed (nor the function con)
  auto vm = vm_weak.lock();
  auto con = con_weak.lock();
  //
  if (vm and con) {
    // Create (empty) private stack and use it for this context.
    CallStack callstack;
    vm->callstack.swap(callstack);
  
    auto res = con;
    process_args(vm, res, forward<ArgTypes>(args)...);
    auto result = vm->ret;

    // Restore the original stack
    vm->callstack.swap(callstack);

    return unbox<typename FromNative<RetType>::type>(result);

  } else {
    return RetType(0);
  }
}

} // namespace idris

#endif // __idris_cpp_runtime_callbacks_h_

