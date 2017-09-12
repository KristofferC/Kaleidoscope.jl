##########
# Parser #
##########

mutable struct Parser
    l::Lexer
    current_token::Token
    Parser(str) = new(Lexer(str))
end

current_token(ps::Parser) = ps.current_token
next_token!(ps::Parser) = return (ps.current_token = gettok(ps.l))

# Operator precedence
const BinopPrecedence = Dict{String,Int}()
BinopPrecedence["<"] = 10
BinopPrecedence[">"] = 10
BinopPrecedence["+"] = 20
BinopPrecedence["-"] = 20
BinopPrecedence["*"] = 40
BinopPrecedence["/"] = 40

function GetTokPrecedence(ps)
    v = current_token(ps).val
    return (v in keys(BinopPrecedence)) ? BinopPrecedence[v] : -1
end

#############
# AST Nodes #
#############

abstract type ExprAST end

struct NumberExprAST <: ExprAST
    val::Float64
end
Base.show(io::IO, expr::NumberExprAST) = print(io, expr.val)

struct VariableExprAST <: ExprAST
    name::String
end
Base.show(io::IO, expr::VariableExprAST) = print(io, expr.name)

struct BinaryExprAST <: ExprAST
    op::String
    lhs::ExprAST
    rhs::ExprAST
end
Base.show(io::IO, expr::BinaryExprAST) = print(io, "(", expr.lhs, ")", expr.op, "(", expr.rhs, ")")

struct CallExprAST <: ExprAST
    callee::String
    args::Vector{ExprAST}
end

struct IfExprAST <: ExprAST
    cond::ExprAST
    then::ExprAST
    elsee::ExprAST
end

struct ForExprAST <: ExprAST
    varname::String
    start::ExprAST
    endd::ExprAST
    step::ExprAST
    body::ExprAST
end

struct PrototypeAST
    name::String
    args::Vector{String}
end

struct FunctionAST
    proto::PrototypeAST
    body::ExprAST
end



#####################
# Parse Expressions #
#####################

function ParseNumberExpr(ps::Parser)::NumberExprAST
    result = NumberExprAST(Base.parse(Float64, current_token(ps).val))
    next_token!(ps)
    return result
end

function ParseIdentifierExpr(ps::Parser)::Union{ VariableExprAST, CallExprAST}
    IdName = current_token(ps).val
    
    next_token!(ps)
    if current_token(ps).val != "("
        return VariableExprAST(IdName)
    end

    next_token!(ps) # eat '('
    args = ExprAST[]
    while true
        push!(args, ParseExpression(ps))
        if current_token(ps).val == ")"
            break
        end
        if current_token(ps).val != ","
            error("Expected ')' or ',' in argument list")
        end
        next_token!(ps) # eat the ','
    end
    next_token!(ps) # eat ')'
    return CallExprAST(IdName, args)
end

function ParseIfExpr(ps)::IfExprAST
    next_token!(ps) # eat 'if'
    cond = ParseExpression(ps)

    if current_token(ps).kind != tok_then
        error("expected 'then'")
    end
    next_token!(ps) # eat then
    then = ParseExpression(ps)

    if current_token(ps).kind != tok_else
        error("expected 'else'")
    end
    next_token!(ps)
    elsee = ParseExpression(ps)

    return IfExprAST(cond, then, elsee)
end

function ParsePrototype(ps)::PrototypeAST
    if current_token(ps).kind != tok_identifier
        error("Expected function name in prototype")
    end

    FnName = current_token(ps).val
    tok = next_token!(ps) # eat identifier

    if tok.val != "("
        error("Expected '(' in prototype")
    end

    argnames = String[]
    while (next_token!(ps).kind == tok_identifier)
        push!(argnames, current_token(ps).val)
    end

    if current_token(ps).val != ")"
        error("Expected ')' in prototype, got $(current_token(ps))")
    end

    next_token!(ps)

    return PrototypeAST(FnName, argnames)
end

function ParseDefinition(ps)::FunctionAST
    next_token!(ps) # eat def
    proto = ParsePrototype(ps)
    E = ParseExpression(ps)
    return FunctionAST(proto, E)
end

function ParseParenExpr(ps::Parser)::ExprAST
    next_token!(ps) # eat '('
    V = ParseExpression(ps)
    if current_token(ps).val != ")"
        error("expected ')'")
    end
    next_token!(ps) # eat ')'
    return V
end

function ParseBinOpRHS(ps, ExprPrec::Int, LHS::ExprAST)::ExprAST
    while true
        tokprec = GetTokPrecedence(ps)
        if tokprec < ExprPrec
            return LHS
        end

        BinOp = current_token(ps)
        next_token!(ps) # eat binary token

        RHS = ParsePrimary(ps)
        nextprec = GetTokPrecedence(ps)
        if tokprec < nextprec 
            RHS = ParseBinOpRHS(ps, tokprec + 1, RHS)
        end

        LHS = BinaryExprAST(BinOp.val, LHS, RHS)
    end
end

function ParseForExpr(ps)::ForExprAST
    next_token!(ps) # eat 'for'

    if current_token(ps).kind != tok_identifier
        error("expected identifier after for")
    end

    idname = current_token(ps).val
    next_token!(ps) # eat identifier

    if current_token(ps).val != "="
        error("expected `=` after identifier in for expression")
    end
    next_token!(ps) # eat =

    start = ParseExpression(ps)
    if current_token(ps).val != ","
        error("expected `,` after for start value")
    end
    next_token!(ps) # eat ,

    endd = ParseExpression(ps)

    # TODO: make optional
    if current_token(ps).val != ","
        error("expected ',' after for end value")
    end
    next_token!(ps) # eat ,
    step = ParseExpression(ps)

    if current_token(ps).kind != tok_in
        error("expected 'in' after for")
    end
    next_token!(ps) # eat in

    body = ParseExpression(ps)

    return ForExprAST(idname, start, endd, step, body)
end

function ParseExtern(ps)::PrototypeAST
    next_token!(ps) # eat 'extern'
    return ParsePrototype(ps)
end

function ParseTopLevelExpr(ps)::FunctionAST
    E = ParseExpression(ps)
    proto = PrototypeAST("__anon_expr", String[])
    return FunctionAST(proto, E)
end


@noinline function ParseExpression(ps)::ExprAST
    LHS = ParsePrimary(ps)
    return ParseBinOpRHS(ps, 0, LHS)
end

function ParsePrimary(ps)::ExprAST
    curtok = current_token(ps)
    if curtok.kind == tok_identifier
        return ParseIdentifierExpr(ps)
    elseif curtok.kind == tok_number
        return ParseNumberExpr(ps)
    elseif curtok.val == "("
        return ParseParenExpr(ps)
    elseif curtok.kind == tok_if
        return ParseIfExpr(ps)
    elseif curtok.kind == tok_for
        return ParseForExpr(ps)
    else
        error("unknown token: $curtok")
    end
end
