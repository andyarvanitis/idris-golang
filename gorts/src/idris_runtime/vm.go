package idris_runtime

import . "reflect"

//-------------------------------------------------------------------------------------------------
// Data structures/types
//-------------------------------------------------------------------------------------------------

type VirtualMachine struct {
  ValueStack []interface{}
  ValueStackTop int
  ValueStackBase int
  ReturnValue interface{}
  CallStack []CallPair
}

type vmFunction func(vm *VirtualMachine, oldbase int)

type CallPair struct {
  fn vmFunction
  base int
}

//-------------------------------------------------------------------------------------------------
// Virtual machine functions
//-------------------------------------------------------------------------------------------------

func Slide(vm *VirtualMachine, num_args int) {
  for i := 0; i < num_args; i++ {
    (*vm).ValueStack[(*vm).ValueStackBase + i] = (*vm).ValueStack[(*vm).ValueStackTop + i]
  }
}

func Project(vm *VirtualMachine, value interface{}, loc int, arity int) {
  args := ValueOf(value).Interface().(Con).args
  for i := 0; i < arity; i++ {
    (*vm).ValueStack[(*vm).ValueStackBase + i + loc] = args[i]
  }
}

func Reserve(vm *VirtualMachine, size int) {
  // Note: size of zero is prevented in Codegen
  newStack := make([]interface{}, (*vm).ValueStackTop + size + 2) // TODO: why +2?
  copy(newStack, (*vm).ValueStack[:(*vm).ValueStackTop])
  (*vm).ValueStack = newStack
}

func Call(vm *VirtualMachine, fn vmFunction, base int) {
  fn(vm, base)
  for length := len((*vm).CallStack); length > 0; length = len((*vm).CallStack) {
    top := (*vm).CallStack[length - 1]
    function := top.fn
    base := top.base
    (*vm).CallStack = (*vm).CallStack[:length-1]
    function(vm, base)
  }
}

func TailCall(vm *VirtualMachine, fn vmFunction, base int) {
   (*vm).CallStack = append((*vm).CallStack, CallPair{fn, base})
}
