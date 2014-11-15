package idris_runtime

import . "reflect"
import . "os"
import . "strconv"
import . "bufio"
import   "math/big"


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
  tag uintptr
  args []interface{}
}

func MakeCon(tag uintptr, args ...interface{}) Con {
  return Con{tag, args}
}

const invalidTag = ^uintptr(0)

func GetTag(con interface{}) uintptr {
  if con != nil {
    return ValueOf(con).Interface().(Con).tag
  } else {
    return invalidTag
  }
}


func BoolToInt(isTrue bool) int {
  if isTrue {
    return 1
  } else {
    return 0
  }
}

func StringToInt(s string) int64 {
  value, _ := ParseInt(s, 0, 64)
  return value
}

func StringToFloat(s string) float64 {
  value, _ := ParseFloat(s, 64)
  return value
}

func BigIntFromString(n string) *big.Int {
  intResult, _ := big.NewInt(0).SetString(n, 0)
  return intResult
}

var ConstBigZero = big.NewInt(0)
var ConstBigOne  = big.NewInt(1)


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

func Call(vm *VirtualMachine, fn func(vm *VirtualMachine, oldbase uintptr), base uintptr) {
  fn(vm, base)
  for length := len((*vm).CallStack); length > 0; length = len((*vm).CallStack) {
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


func FileOpen(name string, mode string) *File {
  flags := 0
  for _, char := range mode { // TODO: these need some work
    switch char {
      case 'r': flags |= O_RDONLY
      case 'w': flags |= O_RDWR|O_TRUNC|O_CREATE
      case 'a': flags |= O_APPEND|O_CREATE
      case '+': flags |= O_RDWR
    }
    if flags & (O_RDWR|O_APPEND) != 0 {
      flags &^= O_RDONLY
    }
  }
  file, _ := OpenFile(name, flags, 0644)
  return file
}


func FileReadLine(file *File) string {
  // Save off current seek position
  offset, error := file.Seek(0, SEEK_CUR)
  if error == nil {
    reader := NewReader(file)
    line, error := reader.ReadString('\n')
    if error == nil {
      // Set seek position, since it's no longer correct
      file.Seek(offset + int64(len(line)), SEEK_SET)
      return line
    }
  }
  return ""
}

func FileEOF(file *File) int {
  info, error := file.Stat()
  if error == nil {
    size := info.Size()
    offset, error := file.Seek(0, SEEK_CUR)
    if error == nil {
      if offset == size {
        file.Seek(offset + 1, SEEK_SET)
      } else if offset > size {
        return 1
      }
    }
  }
  return 0
}
