
#include <cassert>
#include "vm.h"

namespace idris {

//---------------------------------------------------------------------------------------

void slide(shared_ptr<VirtualMachine>& vm,
           const size_t num_args) {
  for (auto i=0; i < num_args; i++) {
    vm->valstack[vm->valstack_base + i] = move(vm->valstack[vm->valstack_top + i]);
  }
}

void project(shared_ptr<VirtualMachine>& vm,
             const Value& value, const IndexType loc, const int arity) {
  assert(value and value->getTypeId() == Con::typeId);
  auto & args = unbox<Con>(value).args;
  for (auto i = 0; i < arity; i++) {
    vm->valstack[vm->valstack_base + i + loc] = args[i];
  }
}

void reserve(shared_ptr<VirtualMachine>& vm, size_t size) {
  vm->valstack.resize(size + 1, nullptr); // also shrinks the stack, if appropriate
}

void vm_call(shared_ptr<VirtualMachine>& vm,
             const Func& fn, const IndexType base) {
  fn(vm, base);
  while (vm->callstack.size() > 0) {
    auto callstackFn     = get<0>(vm->callstack.top());
    auto callstackFnBase = get<1>(vm->callstack.top());
    vm->callstack.pop();
    callstackFn(vm, callstackFnBase);
  };
}

void vm_tailcall(shared_ptr<VirtualMachine>& vm,
                 const Func& fn, const IndexType base) {
   vm->callstack.push({fn,base});
}

} // namespace idris
