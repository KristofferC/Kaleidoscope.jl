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
    push!(cg.jit, cg.mod)
    initialize_module_and_pass_manager!(cg)

    f_nullable = LLVM.findfunction(cg.jit, "__anon_expr")
    if isnull(f_nullable)
        error("Did not find function in execution engine")
    end
    f = get(f_nullable)
    res = LLVM.run(cg.jit, f)
    println("Got ", convert(Float64, res, LLVM.DoubleType()))
    LLVM.dispose(res)

    return ir
end

function repl()
    reset_timer!(to)
    cg = CodeGen()
    initialize_module_and_pass_manager!(cg)
    while true
        print("ready> ")
        str = readline()
        ps = Parser(str)
        while true
            tok = next_token!(ps)
            if tok.kind == tok_eof
                break
            elseif tok.val == ";"
                next_token!(ps)
            elseif tok.kind == tok_def
                HandleDefinition(ps, cg)
            elseif tok.kind == tok_extern
                HandleExtern(ps, cg)
            else
                HandleTopLevelExpression(ps, cg)
            end
        end
    end

    # TODO: dispose
end
