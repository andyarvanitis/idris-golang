### Experimental Google Go backend for Idris

### Some motivations for this
* I felt like improving my Haskell and learning Go
* ???
* Profit

### Some features/benefits
* Easy interop with Go, as well as with C via cgo (already supported)
* UTF-8 support
* Callbacks from Go (into Idris) support
* With no tweaking yet, performance seems quite good -- almost as fast as C backend

### Niceties/notes
* Go compiles pretty fast, which is nice when using it for a language backend
* Go has a reasonably-well-performing GC (which is continually being improved)
* Go has built-in unicode support (used by this backend)
* Go has standard lib big int support (used by this backend)
* Go has nice reflection features (used by this backend, made things pretty easy)
* No Go third-party libraries needed or used for this backend
* Most of the official tests run successfully -- see the [Makefile](https://github.com/andyarvanitis/idris-golang/blob/master/Makefile)

### Some code examples
* UTF-8 support, so this works and produces "βγδ" as output (the C backend doesn't yet):
```Idris
module Main

greek : String
greek = "αβγδ"

main : IO ()
main = do
  putStrLn "Running Idris main"
  putStrLn $ "Greek: " ++ (strTail greek)
```

* Calling a Go function via the FFI
```Idris
module Main

%include go "fmt"

goprint : String -> IO ()
goprint s = mkForeign (FFun "fmt.Println(%0)" [FString] FUnit) s

main : IO ()
main = do
  goprint "Hello, world!"
```

* Calling a C function via the FFI (via cgo)
```Idris
module Main

%include go "// #include <stdio.h>"
%include go "C"

c_putchar : Char -> IO Int
c_putchar c = mkForeign (FFun "C.putchar(C.int(%0))" [FChar] FInt) c

main : IO ()
main = do

  _ <- c_putchar('B')
  _ <- c_putchar('\n')

  return ()
