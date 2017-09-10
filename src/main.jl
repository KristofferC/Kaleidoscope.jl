function HandleDefinition(ps, cg)
    r = ParseDefinition(ps)
    println("Parsed a function definition")
    ir = codegen(cg, r)
    println(ir)
    return ir
end

function HandleExtern(ps, cg)
    r = ParseExtern(ps)
    println("Parsed an extern")
    ir = codegen(cg, r)
    println(ir)
    return ir
end

function HandleTopLevelExpression(ps, cg)
    r = ParseTopLevelExpr(ps)
    println("Parsed a top-level expr")
    ir = codegen(cg, r)
    println(ir)
    return ir
end

function repl()
    cg = CodeGen()
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
end
