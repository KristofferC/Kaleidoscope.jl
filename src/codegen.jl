mutable struct CodeGen
    context::LLVM.Context
    builder::LLVM.Builder
    jit::LLVM.ExecutionEngine
    namedvalues::Dict{String, LLVM.AllocaInst}
    function_protos::Dict{String, PrototypeAST}
    mod::LLVM.Module
    pass_manager::LLVM.FunctionPassManager
    function CodeGen(context::LLVM.Context, builder::LLVM.Builder, jit::LLVM.ExecutionEngine,
                     namedvalues::Dict{String, LLVM.AllocaInst}, function_protos::Dict{String, PrototypeAST})
        new(context, builder, jit, function_protos, namedvalues)
    end
end

function CodeGen()
    ctx = LLVM.Context()
    builder = LLVM.Builder(ctx)
    jit = LLVM.JIT(LLVM.Module(""))
    return CodeGen(
        ctx,
        builder,
        jit,
        Dict{String, LLVM.AllocaInst}(),
        Dict{String, PrototypeAST}(),
    )
end

Base.show(io::IO, cg::CodeGen) = print(io, "CodeGen")

function initialize_module_and_pass_manager!(cg::CodeGen)
    cg.mod = LLVM.Module("KaleidoscopeModule")
    cg.pass_manager = LLVM.FunctionPassManager(cg.mod)
    LLVM.instruction_combining!(cg.pass_manager)
    LLVM.reassociate!(cg.pass_manager)
    LLVM.gvn!(cg.pass_manager)
    LLVM.cfgsimplification!(cg.pass_manager)
    LLVM.promote_memory_to_register!(cg.pass_manager)
    LLVM.initialize!(cg.pass_manager)
    return cg
end

function get_function(cg::CodeGen, name::String)
    if haskey(LLVM.functions(cg.mod), name)
        return LLVM.functions(cg.mod)[name]
    end

    if haskey(cg.function_protos, name)
        return codegen(cg, cg.function_protos[name])
    end

    error("I guess?")
end

function create_entry_block_allocation(fn::LLVM.Function, varname::String)
    local alloc
    LLVM.Builder() do builder
        entry_block = LLVM.entry(fn)
        LLVM.position!(builder, entry_block)
        alloc = LLVM.alloca!(builder, LLVM.DoubleType(), varname)
    end
    return alloc
end

function codegen(cg::CodeGen, expr::NumberExprAST)::LLVM.Value
    return LLVM.ConstantFP(LLVM.DoubleType(), expr.val)
end

function codegen(cg::CodeGen, expr::VariableExprAST)::LLVM.Value
    # TODO: Error handling if argument is not found
    if !haskey(cg.namedvalues, expr.name)
        error("did not find variable $(expr.name)")
    end
    V = cg.namedvalues[expr.name]
    return LLVM.load!(cg.builder, V, expr.name)
end

function codegen(cg::CodeGen, expr::BinaryExprAST)::LLVM.Value
    if expr.op == Kinds.EQUAL
        LHS = expr.lhs
        if !(LHS isa VariableExprAST)
            error("destination of '=' must be a variable")
        end
        R = codegen(cg, expr.rhs)

        if !haskey(cg.namedvalues, LHS.name)
            error("unknown variable name $(LHS.name)")
        end

        LLVM.store!(cg.builder, R, cg.namedvalues[LHS.name])
        return R
    end
    L = codegen(cg, expr.lhs)
    R = codegen(cg, expr.rhs)

    if expr.op == Kinds.PLUS
        return LLVM.fadd!(cg.builder, L, R, "addtmp")
    elseif expr.op == Kinds.MINUS
        return LLVM.fsub!(cg.builder, L, R, "subtmp")
    elseif expr.op == Kinds.STAR
        return LLVM.fmul!(cg.builder, L, R, "multmp")
    elseif expr.op == Kinds.SLASH
        return LLVM.fdiv!(cg.builder, L, R, "divtmp")
    elseif expr.op == Kinds.LESS
        L = LLVM.fcmp!(cg.builder, LLVM.API.LLVMRealOLT, L, R, "cmptmp")
        return LLVM.uitofp!(cg.builder, L, LLVM.DoubleType(), "booltmp")
    elseif expr.op == Kinds.GREATER
        L = LLVM.fcmp!(cg.builder, LLVM.API.LLVMRealOGT, L, R, "cmptmp")
        return LLVM.uitofp!(cg.builder, L, LLVM.DoubleType(), "booltmp")
    else
        error("Unhandled binary operator $(expr.op)")
    end
end

function codegen(cg::CodeGen, expr::CallExprAST)::LLVM.Value
    func = get_function(cg, expr.callee)

    if length(LLVM.parameters(func)) != length(expr.args)
        error("number of parameters mismatch")
    end

    args = LLVM.Value[]
    for v in expr.args
        push!(args, codegen(cg, v))
    end

    return LLVM.call!(cg.builder, func, args, "calltmp")
end

function codegen(cg::CodeGen, expr::PrototypeAST)::LLVM.Value
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
        func = LLVM.Function(cg.mod, expr.name, func_type)
        LLVM.linkage!(func, LLVM.API.LLVMExternalLinkage)

        for (i, param) in enumerate(LLVM.parameters(func))
            LLVM.name!(param, expr.args[i])
        end
    end
    return func
end

function codegen(cg::CodeGen, expr::FunctionAST)::LLVM.Value
    cg.function_protos[expr.proto.name] = expr.proto

    the_function = get_function(cg, expr.proto.name)
    # TODO: Check that function body is empty?

    entry = LLVM.BasicBlock(the_function, "entry")
    LLVM.position!(cg.builder, entry)

    empty!(cg.namedvalues)
    for (i, param) in enumerate(LLVM.parameters(the_function))
        argname = expr.proto.args[i]
        alloc = create_entry_block_allocation(the_function, argname)
        LLVM.store!(cg.builder, param, alloc)
        cg.namedvalues[argname] = alloc
    end

    body = codegen(cg, expr.body)
    # TODO, delete function on error
    LLVM.ret!(cg.builder, body)
    @show cg.mod
    status = convert(Core.Bool, LLVM.API.LLVMVerifyFunction(LLVM.ref(the_function), LLVM.API.LLVMPrintMessageAction))
    if status
        throw(LLVM.LLVMException("broken function"))
    end

    LLVM.run!(cg.pass_manager, the_function)

    return the_function
end

function codegen(cg::CodeGen, expr::IfExprAST)
    func = LLVM.parent(LLVM.position(cg.builder))
    then = LLVM.BasicBlock(func, "then")
    elsee = LLVM.BasicBlock(func, "else")
    merge = LLVM.BasicBlock(func, "ifcont")

    # if
    cond = codegen(cg, expr.cond)
    zero = LLVM.ConstantFP(LLVM.DoubleType(), 0.0)
    condv = LLVM.fcmp!(cg.builder, LLVM.API.LLVMRealONE, cond, zero, "ifcond")
    LLVM.br!(cg.builder, condv, then, elsee)

    # then
    LLVM.position!(cg.builder, then)
    thencg = codegen(cg, expr.then)
    LLVM.br!(cg.builder, merge)
    then_block = position(cg.builder)

    # else
    LLVM.position!(cg.builder, elsee)
    elsecg = codegen(cg, expr.elsee)
    LLVM.br!(cg.builder, merge)
    else_block = position(cg.builder)

    # merge
    LLVM.position!(cg.builder, merge)
    phi = LLVM.phi!(cg.builder, LLVM.DoubleType(), "iftmp")
    append!(LLVM.incoming(phi), [(thencg, then_block), (elsecg, else_block)])

    return phi
end

function codegen(cg::CodeGen, expr::ForExprAST)
    startblock = position(cg.builder)
    func = LLVM.parent(startblock)

    alloc = create_entry_block_allocation(func, expr.varname)

    start = codegen(cg, expr.start)

    LLVM.store!(cg.builder, start, alloc)

    loopblock = LLVM.BasicBlock(func, "loop")

    LLVM.br!(cg.builder, loopblock)

    LLVM.position!(cg.builder, loopblock)
    variable = LLVM.phi!(cg.builder, LLVM.DoubleType(), expr.varname)
    push!(LLVM.incoming(variable), (start, startblock))

    # TODO: What if not found...
    shadowed_var = false
    if haskey(cg.namedvalues, expr.varname)
        shadowed_var = true
        oldval = cg.namedvalues[expr.varname]
    end
    cg.namedvalues[expr.varname] = variable

    codegen(cg, expr.body)

    step = codegen(cg, expr.step)

    endd = codegen(cg, expr.endd)

    curvar = load!(cg.builder, expr.varname)
    nextvar = LLVM.fadd!(cg.builder, curvar, step, "nextvar")
    LLVM.store!(cg.builder, nextvar, alloc)

    endd = LLVM.fcmp!(cg.builder, LLVM.API.LLVMRealONE, endd,
        LLVM.ConstantFP(LLVM.DoubleType(), 1.0))

    loopendblock = position(cg.builder)
    afterblock = LLVM.BasicBlock(func, "afterloop")

    LLVM.br!(cg.builder, endd, loopblock, afterblock)

    LLVM.position!(cg.builder, afterblock)
    push!(LLVM.incoming(variable), (nextvar, loopendblock))

    if shadowed_var
        cg.namedvalues[expr.varname] = oldval
    else
        delete!(cg.namedvalues, expr.varname)
    end

    return LLVM.ConstantFP(LLVM.DoubleType(), 0.0)
end

function codegen(cg::CodeGen, expr::VarExprAST)
    old_bindings = LLVM.AllocaInst[]
    func = LLVM.parent(LLVM.position(cg.builder))
    for (varname, init) in expr.varnames
        initval = codegen(cg, init)
        alloca = create_entry_block_allocation(func, varname)
        LLVM.store!(cg.builder, initval, alloca)
        if haskey(cg.namedvalues, varname)
            push!(old_bindings, cg.namedvalues[varname])
        end
        @show varname
        cg.namedvalues[varname] = alloca
    end
    body = codegen(cg, expr.body)
    for i in 1:length(old_bindings)
        cg.namedvalues[first(expr.varnames[i])] = old_bindings[i]
    end

    return body

end
