struct CodeGen
    context::LLVM.Context
    builder::LLVM.Builder
    mod::LLVM.Module
    pass_manager::LLVM.FunctionPassManager
    namedvalues::Dict{String, LLVM.Value}
end

function CodeGen()
    ctx = LLVM.Context()
    builder = LLVM.Builder(ctx)
    mod = LLVM.Module("KaleidoscopeModule")
    pass_manager = add_optimizations!(mod)
    return CodeGen(
        ctx,
        builder,
        mod,
        pass_manager,
        Dict{String, LLVM.Value}()
    )
end

function add_optimizations!(mod::LLVM.Module)
    pass_manager = LLVM.FunctionPassManager(mod)
    LLVM.instruction_combining!(pass_manager)
    LLVM.reassociate!(pass_manager)
    LLVM.gvn!(pass_manager)
    LLVM.cfgsimplification!(pass_manager)
    LLVM.initialize!(pass_manager)
    return pass_manager
end

function codegen(cg::CodeGen, expr::NumberExprAST)
    return LLVM.ConstantFP(LLVM.DoubleType(), expr.val)
end

function codegen(cg::CodeGen, expr::VariableExprAST)
    V = cg.namedvalues[expr.name]
    return V
end

function codegen(cg::CodeGen, expr::BinaryExprAST)
    L = codegen(cg, expr.lhs)
    R = codegen(cg, expr.rhs)

    if expr.op == "+"
        return LLVM.fadd!(cg.builder, L, R, "addtmp")
    elseif expr.op == "-"
        return LLVM.fsub!(cg.builder, L, R, "subtmp")
    elseif expr.op == "*"
        return LLVM.fmul!(cg.builder, L, R, "multmp")
    elseif expr.op == "<"
        error("Unhandled <")
    else
        error("Unhandled binary operator $(expr.op)")
    end
end

function codegen(cg::CodeGen, expr::CallExprAST)
    if !haskey(LLVM.functions(cg.mod), expr.callee)
        error("Unknown function $(expr.callee)")
    end

    func = LLVM.functions(cg.mod)[expr.callee]
    
    if length(LLVM.parameters(func)) != length(expr.args)
        error("number of parameters mismatch")
    end

    args = LLVM.Value[]
    for v in expr.args
        push!(args, codegen(cg, v))
    end

    return LLVM.call!(cg.builder, func, args, "calltmp")
end

function codegen(cg::CodeGen, expr::PrototypeAST)
    if haskey(LLVM.functions(cg.mod), expr.name)
        func = LLVM.functions(cg.mod)[expr.name]
        
        if length(LLVM.parameters(func)) != length(expr.args)
            error("number of parameters mismatch")
        end

        if length(LLVM.blocks(func)) != 0
            error("Existing function exists with a body")
        end
    else
        args = [LLVM.DoubleType() for i in 1:length(expr.args)]
        func_type = LLVM.FunctionType(LLVM.DoubleType(), args)
        println("Adding function $(expr.name)")
        func = LLVM.Function(cg.mod, expr.name, func_type)
        LLVM.linkage!(func, LLVM.API.LLVMExternalLinkage)

        for (i, param) in enumerate(LLVM.parameters(func))
            LLVM.name!(param, expr.args[i])
        end
    end
    return func
end

function codegen(cg::CodeGen, expr::FunctionAST)
    the_function =
        if haskey(LLVM.functions(cg.mod), expr.proto.name)
            LLVM.functions(cg.mod)[expr.proto.name]
        else
            codegen(cg, expr.proto)
        end
    
    entry = LLVM.BasicBlock(the_function, "entry")
    LLVM.position!(cg.builder, entry)

    empty!(cg.namedvalues)
    for (i, param) in enumerate(LLVM.parameters(the_function))
        cg.namedvalues[expr.proto.args[i]] = param        
    end 

    body = codegen(cg, expr.body)
    # TODO, delete function on error
    LLVM.ret!(cg.builder, body)
    
    status = convert(Core.Bool, LLVM.API.LLVMVerifyFunction(LLVM.ref(the_function), LLVM.API.LLVMPrintMessageAction))
    if status
        println()
        throw(LLVM.LLVMException("broken function"))
    end

    LLVM.run!(cg.pass_manager, the_function)

    return the_function
end