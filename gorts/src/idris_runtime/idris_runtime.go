package idris_runtime

import . "reflect"


type VirtualMachine struct {
  ValueStack []interface{}
  ValueStackTop uintptr
  ValueStackBase uintptr
  ReturnValue interface{}
  CallStack []CallPair
}

type CallPair struct {
  fn func(vm *VirtualMachine, oldbase uintptr)
  base uintptr
}


type Con struct {
  Tag uintptr
  args []interface{}
}

func MakeCon(tag uintptr, args ...interface{}) Con {
  return Con{Tag: tag, args: args}
}


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
  newsize := size - uintptr(len((*vm).ValueStack))
  if newsize == 1 {
    (*vm).ValueStack = append((*vm).ValueStack, nil)
  } else if newsize > 1 {
    (*vm).ValueStack = append((*vm).ValueStack, make([]interface{}, newsize))
  }
}

func Call(vm *VirtualMachine, fn func(vm *VirtualMachine, oldbase uintptr), base uintptr) {
  fn(vm, base)
  length := len((*vm).CallStack)
  for length > 0 {
    top := (*vm).CallStack[length - 1]
    function := top.fn
    base := top.base
    (*vm).CallStack = (*vm).CallStack[:length-1]
    function(vm, base)
  }
}

func TailCall(vm *VirtualMachine, fn func(vm *VirtualMachine, oldbase uintptr), base uintptr) {
   (*vm).CallStack = append((*vm).CallStack, CallPair{fn, base})
}
