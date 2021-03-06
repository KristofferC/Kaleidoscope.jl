function HandleDefinition(ps, cg)
    r = ParseDefinition(ps)
    ir = codegen(cg, r)
    push!(cg.jit, cg.mod)
    initialize_module_and_pass_manager!(cg)
    return ir
end

function HandleExtern(ps, cg)
    protoAST = ParseExtern(ps)
    ir = codegen(cg, protoAST)
    cg.function_protos[protoAST.name] = protoAST
    return ir
end

function HandleTopLevelExpression(ps, cg)
    r = ParseTopLevelExpr(ps)
    ir = codegen(cg, r)
    oldmod = cg.mod
    push!(cg.jit, cg.mod) do
        initialize_module_and_pass_manager!(cg)
        if !haskey(LLVM.functions(cg.jit), "__anon_expr")
            error("Did not find function in execution engine")
        end
        f = LLVM.functions(cg.jit)["__anon_expr"]
        res = LLVM.run(cg.jit, f)
        println("Got ", convert(Float64, res, LLVM.DoubleType()))
        LLVM.dispose(res)
    end
    LLVM.dispose(oldmod)
    return ir
end

function repl()
    cg = CodeGen()
    initialize_module_and_pass_manager!(cg)
    while true
        print("ready> ")
        str = readline()
        _generate_ir(str, cg)
    end
    # TODO: dispose
end

function generate_ir(str::String)
    cg = CodeGen()
    initialize_module_and_pass_manager!(cg)
    _generate_ir(str, cg)
end

function _generate_ir(str, cg::CodeGen)
    initialize_module_and_pass_manager!(cg)
    ps = Parser(str)
    tok = next_token!(ps)
    while true
        tok = current_token(ps)
        if tok.kind == Kinds.EOF
            break
        elseif tok.kind == Kinds.SEMICOLON
            next_token!(ps)
        elseif tok.kind == Kinds.DEF
            HandleDefinition(ps, cg)
        elseif tok.kind == Kinds.EXTERN
            HandleExtern(ps, cg)
        else
            HandleTopLevelExpression(ps, cg)
        end
    end
    return cg.mod
end

function emit_objectfile(str::String, path::String)
    cg = CodeGen()
    initialize_module_and_pass_manager!(cg)
    host_triple = LLVM.triple()
    host_t = LLVM.Target(host_triple)
    _generate_ir(str, cg)
    LLVM.TargetMachine(host_t, host_triple) do tm
        LLVM.emit(tm, cg.mod, LLVM.API.LLVMObjectFile, path)
    end
end

str = """
def fib(x) {
    if (x < 3) then
        1
    else
        fib(x-1)+fib(x-2)
}

fib(10)
"""

Kaleidoscope.generate_ir(str)

str = """
def fib(x) {
    var a = 1, b = 1
    for i = 3, i < x, 1.0 in {
        var c = a + b
        a = b
        b = c
    }
    b
}

fib(10)
"""

Kaleidoscope.generate_ir(str)
