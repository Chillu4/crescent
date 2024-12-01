--!strict
local types = require("types")

local sym = require("./runtime/sym")
local keyword = require("./runtime/keyword")
local vector = require("./runtime/vector")
local hmap = require("./runtime/hmap")
local list = require("./runtime/list")

local Position = types.Position
local SyntaxObject = types.SyntaxObject
local Span = types.Span
local EOF = types.EOF

type Position = types.Position
type SyntaxObject<T> = types.SyntaxObject<T>
type Span = types.Span
type EOF = types.EOF

type List = list.List
type Vector = vector.Vector
type Hmap = hmap.Hmap
type Keyword = keyword.Keyword

function new_reader(file: string, str: string): Reader
    return {
        src = str,
        offset = 0,
        char = '',
        line = 1,
        column = 0,
        file = file,
    }
end

function current_pos(state: Reader): Position
    return Position(state.line, state.column, state.offset)
end

local function read_err(state: Reader, msg: string): never
    local template = "syntax error at line %d column %d: %s"
    local content = string.format(template, state.line, state.column, msg)
    error(content)
end

function peek(state: Reader, offset: number?): string
    local _offset = state.offset + (offset or 1)
    local char = string.sub(state.src, _offset, _offset)

    return char
end

function read_char(state: Reader): string
    local offset = state.offset + 1
    state.offset = offset

    local char = string.sub(state.src, offset, offset)
    state.char = char
    
    if char == "\r" and peek(state) == "\n" then
        state.offset += 1
        state.line += 1
        state.column = 0
        return "\n"
    elseif char == "\n" then
        state.line += 1
        state.column = 0
        return "\n"
    else
        state.column += 1
    end

    return char
end

function read_number(state: Reader): SyntaxObject<number>
    local symbol, span = read_token(state)
    local str_num = string.gsub(symbol, "_", "")
    local num = tonumber(str_num)

    if num ~= nil then
        return SyntaxObject(num, span, state.file)
    end
    
    return read_err(state, `invalid number {symbol}`)
end

-- Forward declare
local read: <T>(state: Reader) -> SyntaxObject<T>

function read_until(state: Reader, delimiter: string): ({ any }, Span)
    local tbl = {}
    local start_pos = current_pos(state)

    while true do
        local char = peek(state)
        if char == "" then
            read_err(state, `unmatched {delimiter}`)
        end

        if char == delimiter then
            read_char(state)
            local end_pos = current_pos(state)
            return tbl, Span(start_pos, end_pos)
        end

        if char == ' ' or char == '\n' then
            read_char(state)
            continue
        end
        
        local data = read(state)
        if data.datum == EOF then
            read_err(state, `unmatched {delimiter}`)
        end

        table.insert(tbl, data)
    end
end

function read_string(state: Reader): SyntaxObject<string>
    local start_pos = current_pos(state)
    local str = ""

    while true do
        local char = read_char(state)
        if char == "" then
            read_err(state, "EOF while reading string")
        end

        if char == '\\' then
            local nxt = read_char(state)
            if nxt == 'n' then
                str ..= '\n'
            elseif nxt == 't' then
                str ..= '\t'
            elseif nxt == 'a' then
                str ..= '\a'
            elseif nxt == 'r' then
                str ..= '\r'
            elseif nxt == 'v' then
                str ..= '\v'
            elseif nxt == '\\' then
                str ..= '\\'
            elseif nxt == '"' then
                str ..= '"'
            elseif nxt == '\b' then
                str ..= '\b'
            else
                read_err(state, "invalid escape code: \\" .. nxt)
            end

            continue
        end

        if char == '"' then
            local end_pos = current_pos(state)
            return SyntaxObject(str, Span(start_pos, end_pos), state.file) 
        end

        str ..= char
    end
end

function read_list(state: Reader): SyntaxObject<List>
    local val, span = read_until(state, ")")
    return SyntaxObject(list.from_table(val, #val), span, state.file)
end

function read_vector(state: Reader): SyntaxObject<Vector>
    local val, span = read_until(state, "]")
    return SyntaxObject(vector.from_table(val), span, state.file)
end

function read_map(state: Reader): SyntaxObject<Hmap>
    local vals, span = read_until(state, "}")
    if #vals % 2 ~= 0 then
        read_err(state, "missing key/value for map")    
    end

    local acc = {}
    for i = 1, #vals, 2 do
        acc[vals[i]] = vals[i + 1]
    end

    return SyntaxObject(hmap.from_table(acc), span, state.file)
end

function read_token(state: Reader): (string, Span)
    local start_pos = current_pos(state)
    local token = state.char

    while true do
        local char = peek(state)
        if char == "" or char == " "
            or char == "]" or char == "}" or char == ")"
            or char == '[' or char == "{" or char == "("
            or char == '\n' or char == '\r'
        then
            break
        end
        
        -- Regex for characters allowed in symbol:
        -- [a-zA-z0-9`~!@#\$%\^&\*\-_=\+:,<\.>\/\?\\|]
        local ord = string.byte(peek(state))
        if (ord == 33)
            or (ord >= 35 and ord <= 38)
            or (ord >= 42 and ord <= 58)
            or (ord >= 60 and ord <= 90)
            or (ord == 92)
            or (ord >= 94 and ord <= 122)
            or (ord == 124)
            or (ord == 126)
        then
            token ..= read_char(state)
        else
            read_err(state, "invalid character in token: " .. string.byte(char))
        end
    end

    local end_pos = current_pos(state)
    return token, Span(start_pos, end_pos)
end

function read_keyword(state: Reader): SyntaxObject<Keyword>
    read_char(state)
    local name, span = read_token(state)
    return SyntaxObject(keyword(name), span, state.file)
end

local reader_macros = {
    ["'"] = function(state: Reader)
        local stx = read(state)
        local span = stx.span

        local quote = SyntaxObject(sym("quote"), span, state.file)
        return SyntaxObject(list.from_table({ quote, stx }, 2), span, state.file)
    end,
    
    [","] = function(state: Reader)
        if peek(state) == "@" then
            read_char(state)
            local stx = read(state)
            local span = stx.span
            local unquote_splicing = SyntaxObject(sym("unquote-splicing"), span, state.file)
            return SyntaxObject(list.from_table({ unquote_splicing, stx }, 2), span, state.file)
        end

        local stx = read(state)
        local span = stx.span
        local unquote = SyntaxObject(sym("unquote"), span, state.file)
        return SyntaxObject(list.from_table({ unquote, stx }, 2), span, state.file)
    end,
    
    ["`"] = function(state: Reader)
        local stx = read(state)
        local span = stx.span
        
        local syntax_quote = SyntaxObject(sym("syntax-quote"), span, state.file)
        return SyntaxObject(list.from_table({ syntax_quote, stx }, 2), span, state.file)
    end,

    ["#"] = function(state: Reader)
        if peek(state) == "_" then
            read_char(state)
            local stx = read(state)
            local span = stx.span
            
            local comment = SyntaxObject(sym("comment"), span, state.file)
            return SyntaxObject(list.from_table({ comment, stx }, 2), span, state.file)

        elseif peek(state) == "(" then
            read_char(state)
            local stx = read_list(state)
            local span = stx.span

            local short_fn = SyntaxObject(sym("short-fn"), span, state.file)
            return SyntaxObject(list.from_table({ short_fn, stx }, 2), span, state.file)
        end

        local name, span = read_token(state)
        return SyntaxObject(sym(name), span, state.file)
    end
}

function read<T>(state: Reader): SyntaxObject<any>
    local char = read_char(state)
    if char == "" then
        return SyntaxObject(EOF, Span(current_pos(state), current_pos(state)), state.file)
    end

    local specials = {
        ["("] = read_list,
        [")"] = function(state: Reader)
            read_err(state, "unmatched ')'")
        end,
    
        ["["] = read_vector,
        ["]"] = function(state: Reader)
            read_err(state, "unmatched ']'")
        end,
    
        ["{"] = read_map,
        ["}"] = function(state: Reader)
            read_err(state, "unmatched '}'")
        end,
    
        ["\""] = read_string,
        [":"] = read_keyword,

        ["\n"] = function(state: Reader)
            return read(state)
        end,

        [";"] = function(state: Reader)
            while true do 
                local char = read_char(state)
                if char == "\n" or char == "" then
                    break
                end
            end
            return read(state)
        end,

        [" "] = function(state: Reader)
            return read(state)
        end
    }

    if specials[char] then
        return specials[char](state)
    end

    if string.find(char, "%d") or (char == "-" and string.find(peek(state), "%d")) then
        return read_number(state)
    end

    if reader_macros[char] then
        return reader_macros[char](state)
    end

    local token, span = read_token(state)
    if token == "true" then
        return SyntaxObject(true, span, state.file)

    elseif token == "false" then
        return SyntaxObject(false, span, state.file)

    elseif token == "nil" then
        return SyntaxObject(nil, span, state.file)

    else
        return SyntaxObject(sym(token), span, state.file)
    end
end

local function datum_to_stx(datum)
    
end

export type Reader = {
    src: string,
    offset: number,
    char: string,
    line: number,
    column: number,
    file: string
}

return {
    read = function(file: string, src: string)
        local state = new_reader(file, src)
        return read(state)
    end,

    read_until_eof = function(file: string, src: string)
        local state = new_reader(file, src)
        local exprs = {}
    
        while true do
            local stx = read(state)
            if stx.datum == EOF then
                return exprs
            end
    
            table.insert(exprs, stx)
        end
    end,

    tostring = function(data): string
        if type(data) == "string" then
            return '"' .. data .. '"'
        end
    
        return tostring(data)
    end,
}