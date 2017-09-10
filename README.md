# Kaleidoscope

WIP: The Kaleidoscope in Julia using LLVM.jl. Just for learning. Still buggy.

```jl
import Kaleidoscope
Kaleidoscope.repl()
```

```
ready> def test(x) x*2 + 5 + x
Parsed a function definition
Adding function test

define double @test(double %x) {
entry:
  %multmp = fmul double %x, 2.000000e+00
  %addtmp = fadd double %multmp, 5.000000e+00
  %addtmp1 = fadd double %x, %addtmp
  ret double %addtmp1
}

ready> test(5)
Parsing expression...
Parsed a top-level expr
Adding function

define double @0() {
entry:
  %calltmp = call double @test(double 5.000000e+00)
  ret double %calltmp
}
```