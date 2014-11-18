package idris_runtime

import . "reflect"

//-------------------------------------------------------------------------------------------------
// Function used for calls from go into Idris
//-------------------------------------------------------------------------------------------------

func ProxyFunction(vm *VirtualMachine,
                   applyFn vmFunction,
                   con interface{},
                   args ...interface{}) {

  // Create (empty) private stack and use it for this context.
  var savedCallStack []CallPair
  copy(savedCallStack, (*vm).CallStack)
  (*vm).CallStack = make([]CallPair, 0)

  conType := ValueOf(con).Type()
  res := con

  apply := func(arg interface{}) {
    if ValueOf(res).Type() == conType {
      Reserve(vm, (*vm).ValueStackTop + 2)
      (*vm).ValueStack[(*vm).ValueStackTop] = res
      (*vm).ValueStack[(*vm).ValueStackTop + 1] = arg
      oldbase := (*vm).ValueStackBase
      (*vm).ValueStackBase = (*vm).ValueStackTop
      (*vm).ValueStackTop += 2
      Call(vm, applyFn, oldbase)
      res = (*vm).ReturnValue
    }
  }

  for _, arg := range args {
    apply(arg)
  }

  // Specifically for cases of IO functions
  apply(res)

  // Restore the original stack
  copy((*vm).CallStack, savedCallStack)
}
