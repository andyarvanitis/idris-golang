#ifndef __idris_cpp_runtime_lambdas_h_
#define __idris_cpp_runtime_lambdas_h_

//---------------------------------------------------------------------------------------

#define VA_NUM_ARGS(...) VA_NUM_ARGS_(__VA_ARGS__, 10,9,8,7,6,5,4,3,2,1)
#define VA_NUM_ARGS_(_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,N,...) N

#define WRAPPER_VARIANT(func, ...) WRAPPER_VARIANT_(func, VA_NUM_ARGS(__VA_ARGS__))
#define WRAPPER_VARIANT_(func, nargs) WRAPPER_VARIANT__(func, nargs)
#define WRAPPER_VARIANT__(func, nargs) func ## _ ## nargs

#define LAMBDA_WRAPPER(fcon, ...) [](weak_ptr<VirtualMachine> _vm, \
                                     weak_ptr<BoxedValue> _fcon, \
                                     const IndexType _oldbase){ \
  return WRAPPER_VARIANT(LAMBDA_WRAPPER, __VA_ARGS__)(__VA_ARGS__); \
}(vm, fcon, oldbase) // call this outer lambda right now

#define LAMBDA_CAPTURE_LIST _vm, _fcon, _oldbase

#define LAMBDA_WRAPPER_1(R) \
[LAMBDA_CAPTURE_LIST]() -> R { \
  return proxy_function<R>(LAMBDA_CAPTURE_LIST); \
}

#define LAMBDA_WRAPPER_2(A1, R) \
[LAMBDA_CAPTURE_LIST](A1 a1) -> R { \
  return proxy_function<R, A1>(LAMBDA_CAPTURE_LIST,a1); \
}

#define LAMBDA_WRAPPER_3(A1, A2, R) \
[LAMBDA_CAPTURE_LIST](A1 a1, A2 a2) -> R { \
  return proxy_function<R, A1, A2>(LAMBDA_CAPTURE_LIST,a1,a2); \
}

#define LAMBDA_WRAPPER_4(A1, A2, A3, R) \
[LAMBDA_CAPTURE_LIST](A1 a1, A2 a2, A3 a3) -> R { \
  return proxy_function<R, A1, A2, A3>(LAMBDA_CAPTURE_LIST,a1,a2,a3); \
}

#define LAMBDA_WRAPPER_5(A1, A2, A3, A4, R) \
[LAMBDA_CAPTURE_LIST](A1 a1, A2 a2, A3 a3, A4 a4) -> R { \
  return proxy_function<R, A1, A2, A3, A4>(LAMBDA_CAPTURE_LIST,a1,a2,a3,a4); \
}

#define LAMBDA_WRAPPER_6(A1, A2, A3, A4, A5, R) \
[LAMBDA_CAPTURE_LIST](A1 a1, A2 a2, A3 a3, A4 a4, A5 a5) -> R { \
  return proxy_function<R, A1, A2, A3, A4, A5>(LAMBDA_CAPTURE_LIST,a1,a2,a3,a4,a5); \
}

#define LAMBDA_WRAPPER_7(A1, A2, A3, A4, A5, A6, R) \
[LAMBDA_CAPTURE_LIST](A1 a1, A2 a2, A3 a3, A4 a4, A5 a5, A6 a6) -> R { \
  return proxy_function<R, A1, A2, A3, A4, A5, A6>(LAMBDA_CAPTURE_LIST,a1,a2,a3,a4,a5,a6); \
}

#define LAMBDA_WRAPPER_8(A1, A2, A3, A4, A5, A6, A7, R) \
([LAMBDA_CAPTURE_LIST](A1 a1, A2 a2, A3 a3, A4 a4, A5 a5, A6 a6, A7 a7) -> R { \
  return proxy_function<R, A1, A2, A3, A4, A5, A6, A7>(LAMBDA_CAPTURE_LIST,a1,a2,a3,a4,a5,a6,a7); \
}

#define LAMBDA_WRAPPER_9(A1, A2, A3, A4, A5, A6, A7, A8, R) \
([LAMBDA_CAPTURE_LIST](A1 a1, A2 a2, A3 a3, A4 a4, A5 a5, A6 a6, A7 a7, A8 a8) -> R { \
  return proxy_function<R, A1, A2, A3, A4, A5, A6, A7, A8>(LAMBDA_CAPTURE_LIST,a1,a2,a3,a4,a5,a6,a7,a8); \
}

#define LAMBDA_WRAPPER_10(A1, A2, A3, A4, A5, A6, A7, A8, A9, R) \
([LAMBDA_CAPTURE_LIST](A1 a1, A2 a2, A3 a3, A4 a4, A5 a5, A6 a6, A7 a7, A8 a8,a9) -> R { \
  return proxy_function<R, A1, A2, A3, A4, A5, A6, A7, A8, A9>(LAMBDA_CAPTURE_LIST,a1,a2,a3,a4,a5,a6,a7,a8,a9); \
}

#endif // __idris_cpp_runtime_lambdas_h_

