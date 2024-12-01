--!strict
local ir = require("ir")
local types = require("../types")
local errors = require("errors/errors")
local err_fmt = require("errors/format")

local reader_type = require("../types")

local Namespace = require("../runtime/namespace")
local core_ns = require("../runtime/core_ns")
local sym = require("../runtime/sym")
local keyword = require("../runtime/keyword")
local vector = require("../runtime/vector")
local hmap = require("../runtime/hmap")
local list = require("../runtime/list")
local typeof = require("../runtime/typeof")

local compile_err = errors.compile_err
local arity_err = errors.arity_err
local type_err = errors.type_err

local function set(...)
    local tbl = {}
    for i = 1, select("#", ...) do
        tbl[select(i, ...)] = true
    end

    return tbl
end

local binops = set(
    "and", "or", "..", "+", "-", "*", 
    "/", "//", "^", ".", ":", ">", "<", 
    "<=", ">=", "==", "~=", "%"
)

local lua_keywords = set(
    "and", "break", "do", "else", "elseif",
    "end", "false", "for", "function", "goto", 
    "if", "in", "local", "nil", "not",
    "or", "repeat", "return", "then", "true",
    "until", "while"
)

local function obj_name(x): string
    return rawget(x, "__name")
end

local function quote_expr(stx: reader_type.SyntaxObject<any>)
    local datum = stx.datum
    local src_info = {
        span = stx.span,
        file = stx.file
    }
    
    local datum_type = typeof(stx.datum)
    if datum_type == "string" then
        return ir.const(src_info, "string", datum)
    elseif datum_type == "number" then
        return ir.const(src_info, "number", datum)
    elseif datum_type == "boolean" then
        return ir.const(src_info, "boolean", datum)
    elseif datum_type == "nil" then
        return ir.const(src_info, "nil", nil)
    elseif datum_type == "symbol" then
        return ir.const(src_info, "symbol", obj_name(datum))
    elseif datum_type == "keyword" then
        return ir.const(src_info, "keyword", obj_name(datum))
    elseif datum_type == "vector" then
        local vec = {}
        for _, exp_stx in ipairs(datum) do
            table.insert(vec, quote_expr(exp_stx))
        end

        return ir.vector(src_info, vec, #datum)

    elseif datum_type == "hmap" then
        local hmap = {}
        for k, v in pairs(datum) do
            local key = quote_expr(k) 
            local value = quote_expr(v) 
            hmap[key] = value
        end

        return ir.hmap(src_info, hmap)

    elseif datum_type == "list" then
        local acc = {}
        local i = 0
        local current = datum
        while current ~= list.EMPTY_LIST do
            i += 1
            acc[i] = quote_expr(current.head)
            current = current.tail
        end

        return ir.list(src_info, acc, i)
    end

    error("FATAL: invalid type " .. datum_type)
end

local function unwarp_stx(stx: reader_type.SyntaxObject<any>): any
    local datum = stx.datum
    local datum_type = typeof(datum)

    if datum_type == "string" or datum_type == "number"
        or datum_type == "boolean" or datum_type == "nil" 
        or datum_type == "symbol" or datum_type == "keyword" 
    then
        return datum
    elseif datum_type == "vector" then
        local vec = {}
        for _, exp_stx in ipairs(datum) do
            table.insert(vec, unwarp_stx(exp_stx))
        end

        return vector.from_table(vec, #datum)

    elseif datum_type == "hmap" then
        local acc = {}
        for k, v in pairs(datum) do
            local key = unwarp_stx(k) 
            local value = unwarp_stx(v) 
            acc[key] = value
        end

        return hmap.from_table(acc)

    elseif datum_type == "list" then
        local acc = {}
        local i = 0
        local current = datum
        while current ~= list.EMPTY_LIST do
            i += 1   
            acc[i] = unwarp_stx(current.head)
            current = current.tail
        end

        return list.from_table(acc, i)
    end

    error("FATAL: invalid type " .. datum_type)
end

local allowed_metadata = set("hidden", "const", "inline", "macro", "args", "type")
local function parse_metadata(metadata: any)
    local meta = {}

    for tag_stx, val_stx in pairs(metadata) do
        if typeof(tag_stx.datum) ~= "keyword" then
            compile_err(tag_stx.span, "key of metatag should be a keyword")
        end

        local tagname = rawget(tag_stx.datum, "__name")
        if not allowed_metadata[tagname] then
            compile_err(tag_stx.span, "invalid metatag '" .. tagname .. "'")
        end

        -- To prevent cyclic dependency, require the eval function here
        meta[tagname] = require("../main").eval(val_stx)
    end

    return hmap.from_table(meta)
end

local function wrap_datum(datum: any, span, file: string): any
    local datum_type = typeof(datum)
    if datum_type == "string" or datum_type == "number" 
        or datum_type == "nil" or datum_type == "boolean" 
        or datum_type == "symbol" or datum_type == "keyword" 
    then
        return reader_type.SyntaxObject(datum, span, file)

    elseif datum_type == "list" then
        local current = datum
        while current ~= list.EMPTY_LIST do
            rawset(current, "head", wrap_datum(current.head, span, file))
            current = current.tail
        end

        return reader_type.SyntaxObject(datum, span, file)
    elseif datum_type == "vector" then
        for i = 1, #datum do
            datum[i] = wrap_datum(datum[i], span, file)
        end

        return reader_type.SyntaxObject(datum, span, file)
    elseif datum_type == "function" then
        error("cannot return function")
    elseif datum_type == "hmap" then
        local acc = {}
        for k, v in pairs(datum) do
            local key = wrap_datum(k, span, file)
            local value = wrap_datum(v, span, file)
            acc[key] = value
        end

        return reader_type.SyntaxObject(hmap.from_table(acc), span, file)
    end

    error("FATAL: unhandled match for type " .. datum_type)
end

local function find_recur(node)
    local tag = node.tag
    if tag == "fn" or tag == "exec" or tag == "let" then
        return find_recur(node.body[#node.body])
    elseif tag == "ifelse" then
        return find_recur(node.consq) or find_recur(node.falsely)
    elseif tag == "invoke" and node.fn.tag == "var" and node.fn.name == "recur" then
        -- Check if `recur` is invoked anywhere else
        -- (recur ... (recur ...)) is invalid
        return true
    else
        return false
    end
end

local function lower(sym_table: { [string]: true }, stx: reader_type.SyntaxObject<any>)
    local datum = stx.datum
    local src_info = {
        span = stx.span,
        file = stx.file
    }

    local datum_type = typeof(datum)
    if datum_type == "string" then 
        return ir.const(src_info, "string", datum)

    elseif datum_type == "number" then
        return ir.const(src_info, "number", datum)
        
    elseif datum_type == "boolean" then
        return ir.const(src_info, "boolean", datum)

    elseif datum_type == "nil" then
        return ir.const(src_info, "nil", nil)

    elseif datum_type == "keyword" then
        return ir.const(src_info, "keyword", obj_name(datum))

    elseif datum_type == "symbol" then
        local sym_name = obj_name(datum)
        local entry = sym_table[sym_name]

        if type(entry) == "string" then
            return ir.var(src_info, entry)
        elseif entry then
            return ir.var(src_info, sym_name)
        end
        
        local current_ns = Namespace.current_ns()
        local namespace, name = string.match(sym_name, "([^/]*)/(.*)")

        if namespace == nil or namespace == "" then
            namespace, name = current_ns:resolve(sym_name)
            if not namespace then
                errors.resolve_err(src_info.span, sym_name, current_ns.name)
            end

        elseif namespace ~= nil and name == nil then
            compile_err(src_info.span, "invalid symbol name: " .. sym_name)

        elseif namespace == "lua.fenv" then
            if not string.match(name, "[%a_][%w_]*") then
                compile_err(src_info.span, "names of vars from namespace 'lua.fenv' must a valid Lua identifier")
            end

            return ir.native(src_info, name)
        elseif namespace == "lua.op" then
            return ir.dynvar(src_info, namespace, name, nil)
        end

        local box = current_ns:get(namespace, name :: string)
        if not box then
            errors.resolve_err(src_info.span, (namespace :: string) .. "/" .. (name :: string), current_ns.name)
        end

        return ir.dynvar(src_info, namespace, name, box)

    elseif datum_type == "vector" then
        local value = {}
        for _, v in ipairs(datum) do
            table.insert(value, lower(sym_table, v))
        end

        return ir.vector(src_info, value, #datum)

    elseif datum_type == "hmap" then
        local values = {}
        for k, v in pairs(datum) do
            local key = lower(sym_table, k)
            local value =  lower(sym_table, v)
            values[key] = value
        end

        return ir.hmap(src_info, values)

    elseif datum_type == "list" then
        -- We need to distinguish from special forms and invoking a function
        if datum == list.EMPTY_LIST then
            return ir.list(src_info, {}, 0)
        end

        local head_stx = datum.head
        local head_datum = head_stx.datum
        
        local tail_stx = datum.tail -- List<SyntaxObject>
        local argc = #datum.tail

        if typeof(head_datum) == "symbol" then
            local name = obj_name(head_datum)
            
            if name == "if" then
                if argc ~= 3 then
                    arity_err(src_info.span, "if", 3, argc)    
                end

                local test = lower(sym_table, tail_stx.head)
                local consequent = lower(sym_table, tail_stx.tail.head)
                local alternate = lower(sym_table, tail_stx.tail.tail.head)

                return ir.ifelse(src_info, test, consequent, alternate)

            elseif name == "def" then
                --[[
                All possible variations of def:
                    (def name)
                    (def name value)
                    (def name doc-str value)
                    (def name tags value)
                    (def name doc-str tags value)
                ]]
                if argc == 0 or argc > 4 then
                    arity_err(src_info.span, "def", "1..4", argc)
                end

                local identifier_stx = tail_stx.head
                local identifier = identifier_stx.datum
                if typeof(identifier) ~= "symbol" then
                    type_err(identifier_stx.span, 1, "symbol", typeof(identifier))
                end

                local identifier_name = obj_name(identifier)
                if string.match(identifier_name, "[^/]+/.*") then
                   -- TODO: throw error "identifier cannot be namespace qualified"
                    error('')
                end

                local box = Namespace.current_ns():intern(identifier_name)

                if argc == 1 then
                    return ir.def(src_info, nil, identifier_name, nil, {}, box)
                
                elseif argc == 2 then
                    local val = lower(sym_table, tail_stx.tail.head)
                    return ir.def(src_info, nil, identifier_name, val, {}, box)
                    
                elseif argc == 3 then
                    local meta = {}
                    local arg2_stx = tail_stx.tail.head

                    if typeof(arg2_stx.datum) == "string" then
                        meta["doc"] = arg2_stx.datum
                    elseif typeof(arg2_stx.datum) == "hmap" then
                        meta = parse_metadata(arg2_stx.datum)
                    else
                        type_err(arg2_stx.span, 2, "string or hmap", typeof(arg2_stx.datum))
                    end

                    local val = lower(sym_table, tail_stx.tail.tail.head)
                    return ir.def(src_info, nil, identifier_name, val, meta, box)

                elseif argc == 4 then
                    local arg2_stx = tail_stx.tail.head
                    if typeof(arg2_stx.datum) ~= "string" then
                        type_err(arg2_stx.span, 2, "string", typeof(arg2_stx.datum))
                    end
      
                    local arg3_stx = tail_stx.tail.tail.head
                    if typeof(arg3_stx.datum) ~= "hmap" then
                        type_err(arg3_stx.span, 3, "hmap", typeof(arg3_stx.datum))
                    end

                    local meta = parse_metadata(arg3_stx.datum)
                    meta["doc"] = arg2_stx.datum

                    local val = lower(sym_table, tail_stx.tail.tail.tail.head)
                    return ir.def(src_info, nil, identifier_name, val, meta, box)
                end
            elseif name == "let" or name == "letrec" then
                if argc < 2 then
                    arity_err(src_info.span, "let", "2..", argc)    
                end

                -- Do syntax checks for binds
                local recursive = (name == "letrec")

                local bindings_stx = tail_stx.head
                local bindings = bindings_stx.datum
                if typeof(bindings) ~= "vector" then
                    type_err(bindings_stx.span, 1, "vector", typeof(bindings))
                end

                if #bindings % 2 ~= 0 then
                    compile_err(src_info.span, "special " .. name .. " expects the binds vector to be of even length (got length " .. #bindings .. ")")
                end
                
                -- Create a new symbol table
                local new_sym_tbl = table.clone(sym_table)
                local vars = {}
                local last_sym = ""

                -- Add all the binds to the symbol table initially if its recursive
                if recursive then
                    for i, exp_stx in ipairs(bindings) do
                        if i % 2 ~= 0 then
                            local bind_type = typeof(exp_stx.datum)
                            if bind_type ~= "symbol" then
                                type_err(exp_stx.span, i, "symbol", bind_type)
                            end

                            new_sym_tbl[obj_name(exp_stx.datum)] = true
                        end
                    end
                end

                for i, exp_stx in ipairs(bindings) do
                    if i % 2 ~= 0 then
                        local bind_type = typeof(exp_stx.datum)
                        if bind_type ~= "symbol" then
                            type_err(exp_stx.span, i, "symbol", bind_type)
                        end

                        last_sym = obj_name(exp_stx.datum)
                    else
                        -- For letrec, the vars are already added to the symbol table
                        if recursive then
                            table.insert(vars, { last_sym, lower(new_sym_tbl, exp_stx) :: any })
                        else
                            table.insert(vars, { last_sym, lower(new_sym_tbl, exp_stx) :: any })
                            new_sym_tbl[last_sym] = true
                        end
                    end
                end

                local body = {}
                local current = tail_stx.tail
                while current ~= list.EMPTY_LIST do
                    table.insert(body, lower(new_sym_tbl, current.head))
                    current = current.tail
                end

                return ir.let(src_info, recursive, vars, body)

            elseif name == "quote" then
                if argc ~= 1 then
                    arity_err(src_info.span, "quote", 1, argc)    
                end

                local arg1_stx = tail_stx.head
                return quote_expr(arg1_stx)
            
            elseif name == "fn" then
                if argc < 2 then
                    arity_err(src_info.span, "fn", "2..", argc)    
                end

                local arguments_stx = tail_stx.head
                local arguments = arguments_stx.datum
                if typeof(arguments) ~= "vector" then
                    type_err(arguments_stx.span, 1, "vector", typeof(arguments))
                end

                local new_sym_tbl = table.clone(sym_table)
                local args = {}

                -- For now, bring `recur` into scope
                -- In the later pass, we will handle the case of `recur` being misused.
                new_sym_tbl["recur"] = true

                local varargs = nil
                for i, exp_stx in ipairs(arguments) do
                    local exp_type = typeof(exp_stx.datum)
                    if exp_type ~= "symbol" then
                        compile_err(exp_stx.span, `expected argument #{i}'s name to be a symbol, but got {exp_type}`)
                    end

                    local sym_name = obj_name(exp_stx.datum)
                    if sym_name == "&" then
                        local varg_stx = arguments[i + 1]
                        if varg_stx ~= nil and typeof(varg_stx.datum) ~= "symbol" then
                            compile_err(varg_stx.span, `expected argument #{i + 1}'s name to be a symbol, but got {typeof(varg_stx)}`)
                        end

                        if #arguments - 1 > i + 1 then
                            compile_err(src_info.span, `expected one symbol after \`&\`, but received {#arguments - 1}`)
                        end
                        
                        varargs = obj_name(varg_stx.datum)
                        new_sym_tbl[varargs] = true
                        break
                    end

                    new_sym_tbl[sym_name] = true
                    table.insert(args, sym_name)
                end

                local body = {}
                local current = tail_stx.tail
                while current ~= list.EMPTY_LIST do
                    table.insert(body, lower(new_sym_tbl, current.head))
                    current = current.tail
                end

                return ir.fn(src_info, args, varargs, body)
            elseif name == "set!" then
                if argc ~= 2 then
                    arity_err(src_info.span, "set!", 2, argc)    
                end
                
                local identifier_stx = tail_stx.head
                if typeof(identifier_stx.datum) ~= "symbol" then
                    compile_err(src_info.span, "tried to `set!` a non-symbol")
                end

                local identifier = identifier_stx.datum
                if not sym_table[obj_name(identifier)] then
                    compile_err(src_info.span, "tried to `set!` an unbound identifier")
                end

                return ir.setvar(src_info, obj_name(identifier), lower(sym_table, tail_stx.tail.head))
                
            elseif name == "do" then
                local exprs = {}
                local current = tail_stx

                while current ~= list.EMPTY_LIST do
                    table.insert(exprs, lower(sym_table, current.head))
                    current = current.tail
                end

                return ir.exec(src_info, exprs)
            elseif name == "ns" then
                if argc ~= 1 then
                    arity_err(src_info.span, "ns", 1, argc)
                end
                
                local ns_sym_stx = tail_stx.head
                if typeof(ns_sym_stx.datum) ~= "symbol" then
                    type_err(ns_sym_stx.span, 1, "symbol", typeof(ns_sym_stx.datum))
                end

                local ns_name = obj_name(ns_sym_stx.datum)
                local box = core_ns:get("cres.core", "*ns*")
                if not box then
                    -- cres.core/*ns* was not defined, this normally shouldn't happen, but we can define it again.
                    box = core_ns:intern("*ns*", {}, ns_name)
                else
                    box.content = ns_name
                end

                if not Namespace.current_ns() then
                    Namespace.new(ns_name)
                end

                return ir.noop(src_info)
            elseif name == "comment" then
                return ir.noop(src_info)
            end
        end

        -- Not a special form, so lower to invoke
        local fn = lower(sym_table, head_stx)
        
        if fn.tag == "dynvar" and fn.namespace ~= "lua.op" and fn.namespace ~= "lua.fenv" then
            local box = fn.box
            local the_macro = nil

            -- Execute macros
            if box.metadata["macro"] and type(box.content) == "function" then
                the_macro = box.content
            elseif box.metadata["inline"] then
                the_macro = box.metadata["inline"]
                if typeof(the_macro) == "hmap" then
                    the_macro = the_macro[argc] or the_macro["else"]
                elseif type(the_macro) ~= "function" then
                    compile_err(src_info.span, `inline metadata for '{box.name}' should be a function (got type {type(the_macro)})`)
                end
            end

            if the_macro ~= nil then
                local args = {}
                local count = 0
                local current = tail_stx
                while current ~= list.EMPTY_LIST do
                    table.insert(args, unwarp_stx(current.head))
                    count += 1
                    current = current.tail
                end

                local success, result = xpcall(function()
                    return the_macro(unpack(args, 1, count))
                end, err_fmt.traceback)
                if not success then
                    compile_err(src_info.span, `an error occurred while expanding macro '{box.name}'\n\t{err_fmt.format_traceback(result)}`)
                end

                local new_stx = wrap_datum(result, src_info.span, src_info.file)
                local lowered = lower(sym_table, new_stx)
                return lowered
            end
        end

        local args = {}
        local current = tail_stx
        while current ~= list.EMPTY_LIST do
            table.insert(args, lower(sym_table, current.head))
            current = current.tail
        end

        if fn.tag == "dynvar" and fn.namespace == "lua.op" then
            if fn.name == "not" or fn.name == "#" then
                if #args ~= 1 then
                    arity_err(src_info.span, "lua.op/" .. fn.name, 1, #args)    
                end

                return ir.op1(src_info, fn.name, args[1])
            elseif fn.name == ".=" then
                if #args ~= 3 then
                    arity_err(src_info.span, "lua.op/.=", 3, #args)
                end
                
                return ir.setfield(src_info, args[1], args[2], args[3])
            elseif binops[fn.name] ~= nil then
                if fn.name == "-" and #args == 1 then
                    return ir.op1(src_info, "-", args[1])
                elseif #args ~= 2 then
                    arity_err(src_info.span, "lua.op/" .. fn.name, 2, #args)
                end

                return ir.op2(src_info, fn.name, args[1], args[2])
            end

            errors.namespace_err(src_info.span, "could not resolve lua.op/" .. fn.name)
        end

        return ir.invoke(src_info, fn, args)
    end

    error("FATAL: not implemented: " .. datum_type)
end

return lower