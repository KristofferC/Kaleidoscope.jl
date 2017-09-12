const tok_eof = -1

# commands
const tok_def = -2
const tok_extern = -3

# primary
const tok_identifier = -4
const tok_number = -5

# cflow
const tok_if = -6
const tok_then = -7
const tok_else = -8

struct Token
    kind::Int
    val::String
end

function Base.show(io::IO, t::Token)
    if t.kind == tok_eof
        print(io, "EOF")
    elseif t.kind == tok_def
        print(io, "DEF")
    elseif t.kind == tok_extern
        print(io, "EXTERN")
    elseif t.kind == tok_if
        print(io, "IF")
    elseif t.kind == tok_then
        print(io, "THEN")
    elseif t.kind == tok_else
        print(io, "ELSE")
    elseif t.kind == tok_identifier
        print(io, "IDENTIFIER: $(t.val)")
    elseif t.kind == tok_number
        print(io, "NUMBER: $(t.val)")
    else
        print(io, "OTHER: $(t.val)")
    end
end

mutable struct Lexer{IO_t <: IO}
    io::IO_t
    last_char::Char
    buffer_start::Int
    buffer_scratch::IOBuffer
end

Lexer(io::IO) = Lexer(io, ' ', position(io), IOBuffer())
Lexer(str::String) = Lexer(IOBuffer(str))

tokenize(x) = Lexer(x)

# Iterator interface
Base.iteratorsize(::Type{Lexer{IO_t}}) where {IO_t} = Base.SizeUnknown()
Base.iteratoreltype(::Type{Lexer{IO_t}}) where {IO_t} = Base.HasEltype()
Base.eltype(::Type{Lexer{IO_t}}) where {IO_t} = Token

function Base.start(l)
    seek(l.io, l.buffer_start)
    false
end

function Base.next(l::Lexer, ::Any)
    t = gettok(l)
    return t, t.kind == tok_eof
end

Base.done(::Lexer, isdone) = isdone
const EOF_CHAR = convert(Char,typemax(UInt32))
readchar(io::IO) = eof(io) ? EOF_CHAR : read(io, Char)

isvalididentifier(x) = isalpha(x) || isdigit(x)

function gettok(l::Lexer)::Token

    if l.last_char == EOF_CHAR
        return Token(tok_eof, "")
    end

    while isspace(l.last_char)
        l.last_char = readchar(l.io)
    end

    if isalpha(l.last_char)
        write(l.buffer_scratch, l.last_char)
        while true
            l.last_char = readchar(l.io)
            !isvalididentifier(l.last_char) && break
            write(l.buffer_scratch, l.last_char)
        end

        identifier = String(take!(l.buffer_scratch))
        if identifier == "def"
            return Token(tok_def, identifier)
        elseif identifier == "extern"
            return Token(tok_extern, identifier)
        elseif identifier == "if"
            return Token(tok_if, identifier)
        elseif identifier == "then"
            return Token(tok_then, identifier)
        elseif identifier == "else"
            return Token(tok_else, identifier)
        else
            return Token(tok_identifier, identifier)
        end
    end    

    # TODO, this is kinda ugly
    if (isdigit(l.last_char) || l.last_char == '.')
        write(l.buffer_scratch, l.last_char)
        l.last_char = readchar(l.io)
        while isdigit(l.last_char) || l.last_char == '.'
            write(l.buffer_scratch, l.last_char)
            l.last_char = readchar(l.io)
        end
        identifier = String(take!(l.buffer_scratch))
        return Token(tok_number, identifier)
    end

    if l.last_char == '#'
        l.last_char = readchar(l.io)
        # Reads one too much
        while l.last_char != '\n' && l.last_char != '\r'
            l.last_char = readchar(l.io)
        end
        return gettok(l)
    end

    this_char = l.last_char
    l.last_char = readchar(l.io)
    return Token(Int(this_char), string(this_char))
end


