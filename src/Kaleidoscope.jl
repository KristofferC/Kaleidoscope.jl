module Kaleidoscope

import LLVM
using TimerOutputs

const to = TimerOutput()

function __init__()
    LLVM.API.LLVMInitializeNativeAsmPrinter()
    LLVM.API.LLVMInitializeNativeAsmParser()
end

include("lexer.jl")
include("ast.jl")
include("codegen.jl")
include("main.jl")

end # module
