package idris_runtime

import . "reflect"

//-------------------------------------------------------------------------------------------------
// Data structures/types
//-------------------------------------------------------------------------------------------------

type VirtualMachine struct {
  ValueStack []interface{}
  ValueStackTop uintptr
  ValueStackBase uintptr
  ReturnValue interface{}
  CallStack []CallPair
}

type vmFunction func(vm *VirtualMachine, oldbase uintptr)

type CallPair struct {
  fn vmFunction
  base uintptr
}

//-------------------------------------------------------------------------------------------------
// Virtual machine functions
//-------------------------------------------------------------------------------------------------

func Slide(vm *VirtualMachine, num_args uintptr) {
  for i := uintptr(0); i < num_args; i++ {
    (*vm).ValueStack[(*vm).ValueStackBase + i] = (*vm).ValueStack[(*vm).ValueStackTop + i]
  }
}

func Project(vm *VirtualMachine, value interface{}, loc uintptr, arity uintptr) {
  args := ValueOf(value).Interface().(Con).args
  for i := uintptr(0); i < arity; i++ {
    (*vm).ValueStack[(*vm).ValueStackBase + i + loc] = args[i]
  }
}

func Reserve(vm *VirtualMachine, size uintptr) {
  for i := uintptr(len((*vm).ValueStack)); i < size; i++ {
    (*vm).ValueStack = append((*vm).ValueStack, nil)
  }
}

func Call(vm *VirtualMachine, fn vmFunction, base uintptr) {
  fn(vm, base)
  for length := len((*vm).CallStack); length > 0; length = len((*vm).CallStack) {
    top := (*vm).CallStack[length - 1]
    function := top.fn
    base := top.base
    (*vm).CallStack = (*vm).CallStack[:length-1]
    function(vm, base)
  }
}

func TailCall(vm *VirtualMachine, fn vmFunction, base uintptr) {
   (*vm).CallStack = append((*vm).CallStack, CallPair{fn, base})
}
