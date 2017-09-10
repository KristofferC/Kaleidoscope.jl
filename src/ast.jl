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


#############
# AST Nodes #
#############

abstract type ExprAST end

struct NumberExprAST <: ExprAST
    val::Float64
end
Base.show(io::IO, expr::NumberExprAST) = print(io, "Number[", expr.val, "]")

struct VariableExprAST <: ExprAST
    name::String
end
Base.show(io::IO, expr::VariableExprAST) = print(io, "Variable[", expr.name, "]")

struct BinaryExprAST <: ExprAST
    op::String
    lhs::ExprAST
    rhs::ExprAST
end
Base.show(io::IO, expr::BinaryExprAST) = print(io, "BinOp[(", expr.lhs, ")", expr.op, "(", expr.rhs, ")]")


struct CallExprAST <: ExprAST
    callee::String
    args::Vector{ExprAST}
end

struct ProtoTypeAST
    name::String
    args::Vector{String}
end

struct FunctionAST
    proto::ProtoTypeAST
    body::ExprAST
end


#####################
# Parse Expressions #
#####################

function ParseNumberExpr(ps::Parser)
    result = NumberExprAST(Base.parse(Float64, current_token(ps).val))
    next_token!(ps)
    println("parsed $result")
    return result
end

function ParseIdentifierExpr(ps::Parser)
    IdName = current_token(ps).val
    @show current_token(ps).val 
    
    next_token!(ps)
    @show current_token(ps).val 
    if current_token(ps).val != "("
        return VariableExprAST(IdName)
    end

    next_token!(ps) # eat '('
    args = ExprAST[]
    while true
        println("Parsing expression...")
        push!(args, ParseExpression(ps))
        if current_token(ps).val == ")"
            break
        end
        @show  current_token(ps).val
        if current_token(ps).val != ","
            error("Expected ')' or ',' in argument list")
        end
        next_token!(ps) # eat the ','
    end
    next_token!(ps) # eat ')'
    return CallExprAST(IdName, args)
end


function ParsePrototype(ps)
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
        error("Expected ')' in prototype")
    end

    next_token!(ps)

    return ProtoTypeAST(FnName, argnames)
end

function ParseDefinition(ps)
    next_token!(ps) # eat def
    proto = ParsePrototype(ps)
    E = ParseExpression(ps)
    return FunctionAST(proto, E)
end

function ParseParenExpr(ps::Parser)
    next_token!(ps) # eat '('
    V = ParseExpression(ps)
    if current_token(ps).val != ")"
        error("expected ')'")
    end
    next_token!(ps) # eat ')'
    return V
end

function ParseTopLevelExpr(ps)
    E = ParseExpression(ps)
    proto = ProtoTypeAST("", String[])
    return FunctionAST(proto, E)
end
    
@noinline function ParsePrimary(ps)
    curtok = current_token(ps)
    if curtok.kind == tok_identifier
        return ParseIdentifierExpr(ps)
    elseif curtok.kind == tok_number
        return ParseNumberExpr(ps)
    elseif curtok.val == "("
        return ParseParenExpr(ps)
    else
        error("unknown token")
    end
end

function HandleDefinition(ps)
    r = ParseDefinition(ps)
    println("Parsed a function definition")
    return r
end


function HandleExtern(ps)
    r = ParseExtern(ps)
    println("Parsed an extern")
    return r
end


function HandleTopLevelExpression(ps)
    r = ParseTopLevelExpr(ps)
    println("Parsed a top-level expr")
    return r
end

function MainLoop()
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
                @show HandleDefinition(ps)
            elseif tok.kind == tok_extern
                @show HandleExtern(ps)
            else
                @show HandleTopLevelExpression(ps)
            end
        end
    end
end

@noinline function ParseExpression(ps)
    LHS = ParsePrimary(ps)
    @show LHS
    return ParseBinOpRHS(ps, 0, LHS)
end

function ParseBinOpRHS(ps, ExprPrec::Int, LHS::ExprAST)
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

function ParseExtern(ps)
    next_token!(ps) # eat 'extern'
    return ParsePrototype(ps)
end

function GetTokPrecedence(ps)
    v = current_token(ps).val
    return (v in keys(BinopPrecedence)) ? BinopPrecedence[v] : -1
end

const BinopPrecedence = Dict{String,Int}()
BinopPrecedence["<"] = 10
BinopPrecedence["+"] = 20
BinopPrecedence["-"] = 20
BinopPrecedence["*"] = 40
