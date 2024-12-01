--!strict
local Namespace = require("../runtime/namespace")
local z = require("z")
local gensym = require("gensym")
local ir = require("ir")
local errors = require("errors/errors")
local sourcemap = require("sourcemap")
local core_ns = require("../runtime/core_ns")

local COMPILE_OPTS = {
    -- Adds arity check to every function created in crescent.
    -- Throws ArityError if the arity doesn't match the arity count of the function.
    arity_check = true,

    -- Optimizes tail-calls by replacing them with loops
    -- Tailcalls are done by invoking the `recur` symbol instead of the symbol the function is stored in.
    -- Can also be used if the function isn't bound to anything.
    tailcall_optimize = true,

    -- Optimize reading of rest arguments of a function by using select instead of head/tail calls
    rest_args_optimize = true
}

-- Store constructors of different types
local to_sym = core_ns:get("cres.core", "sym")
local to_kw = core_ns:get("cres.core", "keyword")
local table_to_vec = core_ns:get("cres.core", "table->vec")
local table_to_hmap = core_ns:get("cres.core", "table->hmap")
local table_to_list = core_ns:get("cres.core", "table->list")

-- Pre-compute mangled names for type constructors
local mangled_sym = z.encode("cres.core/sym")
local mangled_kw = z.encode("cres.core/keyword")
local mangled_vec = z.encode("cres.core/table->vec")
local mangled_hmap = z.encode("cres.core/table->hmap")
local mangled_list = z.encode("cres.core/table->list")

local lua_keywords = {
    "and", "break", "do", "else", "elseif",
    "end", "false", "for", "function", "goto", 
    "if", "in", "local", "nil", "not",
    "or", "repeat", "return", "then", "true",
    "until", "while"
}

local function sanitize_name(str: string): string
    for _, keyword in lua_keywords do
        if string.match(str, "^_*" .. keyword .. "$") then
            return string.gsub(str, keyword, "_" .. keyword)
        end
    end
    
    return str
end

local function compile_const(expr: any, dynenv: any): string
    sourcemap.gensource(expr.span)
    local src = ""

    local typ = expr.type
    if typ == "number" or typ == "boolean" or typ == "nil" then
        src = tostring(expr.value)
    elseif typ == "string" then
        local escaped_str = string.gsub(expr.value, '"', '\\"')
        src = '"' .. escaped_str .. '"'
    else
        if typ == "symbol" then
            if not dynenv[mangled_sym] then
                dynenv[mangled_sym] = to_sym
            end
            
            src = mangled_sym .. ".content(\"" .. expr.value .. "\")"
    
        elseif typ == "keyword" then
            if not dynenv[mangled_kw] then
                dynenv[mangled_kw] = to_kw
            end
            
            src = mangled_kw .. ".content(\"" .. expr.value .. "\")"
        else
            error("could not compile const for type " .. typ)
        end
    end

    return src
end

local function compile_hmap(expr: any, dynenv: any): string
    sourcemap.gensource(expr.span)

    local acc = {}
    for k, v in pairs(expr.elements) do
        local key = compile_expr(k, dynenv)
        local value = compile_expr(v, dynenv)
        table.insert(acc, "[" .. key .. "] =\n" .. value)
    end

    if not dynenv[mangled_hmap] then
        dynenv[mangled_hmap] = table_to_hmap
    end

    local src
    if #acc == 0 then
        src = mangled_hmap .. ".content({})"
    else
        src = mangled_hmap .. ".content({\n" .. table.concat(acc, ",\n") .. "})"
    end

    return src
end

local function compile_vec(expr: any, dynenv: any): string
    sourcemap.gensource(expr.span)

    local acc = {}
    for _, v in ipairs(expr.elements) do
        table.insert(acc, compile_expr(v, dynenv))
    end

    if not dynenv[mangled_vec] then
        dynenv[mangled_vec] = table_to_vec
    end

    local src
    if #acc == 0 then
        src = mangled_vec .. ".content({}, " .. expr.length .. ")"  
    else
        src = mangled_vec .. ".content({\n" .. table.concat(acc, ",\n") .. "}, " .. expr.length .. ")"  
    end

    return src
end

local function compile_list(expr: any, dynenv: any): string
    sourcemap.gensource(expr.span)

    local acc = {}
    for _, v in ipairs(expr.elements) do
        table.insert(acc, compile_expr(v, dynenv))
    end

    if not dynenv[mangled_list] then
        dynenv[mangled_list] = table_to_list
    end

    local src
    if #acc == 0 then
        src = mangled_list .. ".content({}, " .. expr.length .. ")"
    else
        src = mangled_list .. ".content({\n" .. table.concat(acc, ",\n") .. "}, " .. expr.length .. ")"
    end

    return src
end

local function compile_var(expr: any, dynenv: any): string
    sourcemap.gensource(expr.span)

    local src = z.encode(expr.name)
    
    return sanitize_name(src)
end

local function compile_native(expr: any, dynenv: any): string
    sourcemap.gensource(expr.span)
        
    local src = z.encode(expr.value)
    return src
end

local function compile_invoke(expr: any, dynenv: any): string
    local fn = compile_expr(expr.fn, dynenv)
    local args = {}
    for _, v in expr.args do
        table.insert(args, compile_expr(v, dynenv))
    end

    local src
    if #args == 0 then
        src = fn .. "()"
    else
        src = fn .. "(\n" .. table.concat(args, ",\n") .. ")"
    end

    return src
end

local function compile_op1(expr: any, dynenv: any): string
    local op = expr.op
    local src
    if op == "not" then
        src = "(not " .. compile_expr(expr.left, dynenv) .. ")"
    elseif op == "#" then
        src = "(#" .. compile_expr(expr.left, dynenv) .. ")"
    elseif op == "-" then
            src = "(-" .. compile_expr(expr.left, dynenv) .. ")"
    else
        error("not implemented: " .. op)
    end

    return src
end

local function compile_op2(expr: any, dynenv: any): string
    local op = expr.op

    local left = compile_expr(expr.left, dynenv)    
    local src

    if op == "." then
        if expr.right.tag  == "const" and expr.right.type == "string" and string.match(expr.right.value, "^[%a_][%w_]*$") ~= nil then
            -- If the string key is a valid Lua identifier, then compile it using the dot accessor
            src = "(" .. left .. ".\n" .. expr.right.value .. ")"
        else
            src = "(" .. left .. "\n[" .. compile_expr(expr.right, dynenv) .. "])"
        end
    else
        src = "(" .. left .. "\n" .. op .. " " .. compile_expr(expr.right, dynenv) .. ")"
    end

    return src
end

local function compile_ifelse(expr: any, dynenv: any): string
    local cond = compile_expr(expr.cond, dynenv)
    local consq = compile_expr(expr.consq, dynenv)
    local alt = compile_expr(expr.alt, dynenv)
    
    if expr.alt.tag == "ifelse" then
        return "if " .. cond .. " then\n" .. consq .. "\nelse" .. alt
    else
        return "if " .. cond .. " then\n" .. consq .. "\nelse " .. alt .. " end"
    end
end

local function compile_setvar(expr: any, dynenv: any): string
    local name = z.encode(expr.name)
    local value = compile_expr(expr.value, dynenv)
    local src = name .. " = " .. value

    return src
end

function compile_exprs(nodes, dynenv: any): { string }
    local exprs = {}
    for i = 1, #nodes - 1 do
        local expr = nodes[i]
        local src = compile_expr(expr, dynenv)
        table.insert(exprs, src .. "; ")
    end

    if #nodes > 0 then
        local last = nodes[#nodes]
        table.insert(exprs, compile_expr(last, dynenv))
    end

    return exprs
end

local function compile_setfield(node: any, dynenv: any): string
    local tbl = compile_expr(node.tbl, dynenv)
    local field = compile_expr(node.field, dynenv)
    local value = compile_expr(node.value, dynenv)
    local src = tbl .. "\n[" .. field .. "] =\n" .. value

    return src
end

local function compile_box(node: any, dynenv: any): string
    sourcemap.gensource(node.span)

    local mangled = z.encode(node.box.name)
    if dynenv[mangled] == nil then
        dynenv[mangled] = node.box
    end

    return mangled
end

local function compile_dynvar(node: any, dynenv: any): string
    return compile_box(node, dynenv) .. ".content"
end

local function compile_exec(node: any, dynenv: any): string
    local exprs = compile_exprs(node.exprs, dynenv)
    return "do " .. table.concat(exprs, "\n") .. " end"
end

local function compile_let(node: any, dynenv: any)
    if node.recursive then
        sourcemap.gensource(node.span)
    end

    local assignments = {}
    local var_names = {}
    for _, v in node.binds do
        local name = sanitize_name(z.encode(v[1]))
        local value_node = v[2]

        local value = compile_expr(value_node, dynenv)
        if node.recursive then
            table.insert(var_names, name)
        else
            name = "local " .. name
        end

        table.insert(assignments, name .. " = " .. value .. ";")
    end

    local body = compile_exprs(node.body, dynenv)

    if #assignments == 0 then
        return "do " .. table.concat(body, "\n") .. " end"
    elseif node.recursive then
        local declarations = "local " .. table.concat(var_names, ", ")
        return "do " .. declarations .. "\n" .. table.concat(assignments, "\n") .. "\n" .. table.concat(body, "\n") .. " end"
    else
        return "do " .. table.concat(assignments, "\n") .. "\n" .. table.concat(body, "\n") .. " end"
    end
end

-- local function alpha_conversion(node: any, from: string, to: string)
--     local tag = node.tag

--     if tag == "const" or tag == "noop" or tag == "dynvar" or tag == "native" then
--         return

--     elseif tag == "vector" or tag == "list" then
--         for i = 1, node.length do
--             alpha_conversion(node.elements[i], from, to)
--         end

--     elseif tag == "hmap" then
--         for k, v in pairs(node.elements) do
--             alpha_conversion(k, from, to)
--             alpha_conversion(v, from, to)
--         end
        
--     elseif tag == "var" then
--         if node.name == from then
--             node.name = to
--         end

--     elseif tag == "ifelse" then
--         alpha_conversion(node.cond, from, to)
--         alpha_conversion(node.consq, from, to)
--         alpha_conversion(node.alt, from, to)

--     elseif tag == "let" then
--         -- Check if the `let` shadows `from`
--         for _, v in ipairs(node.binds) do
--             if v[1] == from then
--                 return
--             end
--         end

--         for _, v in ipairs(node.body) do
--             alpha_conversion(v, from, to)
--         end

--     elseif tag == "invoke" then
--         alpha_conversion(node.fn, from, to)

--         for _, v in ipairs(node.args) do
--             alpha_conversion(v, from, to)
--         end

--     elseif tag == "setvar" then
--         if node.name == from then
--             node.name = to
--         end

--         alpha_conversion(node.value, from, to)

--     elseif tag == "setfield" then
--         alpha_conversion(node.tbl, from, to)
--         alpha_conversion(node.field, from, to)
--         alpha_conversion(node.value, from, to)

--     elseif tag == "def" then
--         alpha_conversion(node.value, from, to)

--     elseif tag == "fn" then
--         -- Check if function arguments shadow `from`
--         if node.varargs == from then
--             return
--         end

--         for _, v: any in ipairs(node.args) do
--             if v == from then
--                 return
--             end
--         end

--         for _, v in ipairs(node.body) do
--             alpha_conversion(v, from, to)
--         end

--     elseif tag == "exec" then
--         for _, v in ipairs(node.exprs) do
--             alpha_conversion(v, from, to)
--         end

--     elseif tag == "op1" then
--         alpha_conversion(node.left, from, to)

--     elseif tag == "op2" then
--         alpha_conversion(node.left, from, to)
--        alpha_conversion(node.right, from, to)
--     else
--         error("invalid ir: " .. tag)
--     end
-- end

local function contains_var(node: any, name: string): boolean
    local tag = node.tag

    if tag == "const" or tag == "noop" or tag == "dynvar" or tag == "native" then
        return false

    elseif tag == "vector" or tag == "list" then
        for i = 1, node.length do
            if contains_var(node.elements[i], name) then
                return true
            end
        end

        return false
    elseif tag == "hmap" then
        for k, v in pairs(node.elements) do
            if contains_var(k, name) or contains_var(v, name) then
                return true
            end
        end
        
        return false
    elseif tag == "var" then
        if node.name == name then
            return true
        end

        return false
    elseif tag == "ifelse" then
        return contains_var(node.cond, name)
            or contains_var(node.consq, name)
            or contains_var(node.alt, name)

    elseif tag == "let" then
        -- Check if the `let` shadows `from`
        for _, v in ipairs(node.binds) do
            if v[1] == name then
                return true
            end
        end

        for _, v in ipairs(node.body) do
            if contains_var(v, name) then
                return true
            end
        end

        return false
    elseif tag == "invoke" then
        if contains_var(node.fn, name) then
            return true
        end

        for _, v in ipairs(node.args) do
            if contains_var(v, name) then
                return true
            end
        end

        return false
    elseif tag == "setvar" then
        if node.name == name then
            return true
        end

        return contains_var(node.value, name)

    elseif tag == "setfield" then
        return contains_var(node.tbl, name)
        or contains_var(node.field, name)
        or contains_var(node.value, name)

    elseif tag == "def" then
        return contains_var(node.value, name)

    elseif tag == "fn" then
        -- Check if function arguments shadow `from`
        if node.varargs == name then
            return true
        end

        for _, v: any in ipairs(node.args) do
            if v == name then
                return true
            end
        end

        for _, v in ipairs(node.body) do
            if contains_var(v, name) then
                return true
            end
        end

        return false
    elseif tag == "exec" then
        for _, v in ipairs(node.body) do
            if contains_var(v, name) then
                return true
            end
        end
        
        return false
    elseif tag == "op1" then
        return contains_var(node.left, name)

    elseif tag == "op2" then
        return contains_var(node.left, name) or contains_var(node.right, name)
    elseif tag == "ret" then
        return contains_var(node.value, name)
    elseif tag == "retvar" then
        if name == node.name then
            return true
        end

        return contains_var(node.value, name)
    elseif tag == "box" then
        return false
    end

    error("invalid ir: " .. tag)
end

local function is_tailcall(node: any, args: { string }): boolean
    local tag = node.tag
    if tag == "fn" then
        for _, arg in node.args do
            if arg == "recur" then
                return false
            end
        end

        return is_tailcall(node.body[#node.body], args)
    elseif tag == "var" and node.name == "recur" then
        errors.compile_err(node.span, "`recur` can only be called at tail position")
        return false
    elseif tag == "let" then
        for _, bind in node.binds do
            if bind[1] == "recur" then
                return false
            end
        end

        return is_tailcall(node.body[#node.body], args)
    elseif tag == "exec" then
        return is_tailcall(node.exprs[#node.exprs], args)
    elseif tag == "ifelse" then
        return is_tailcall(node.consq, args) or is_tailcall(node.alt, args)
    elseif tag == "ret" then
        local value = node.value
        if value.tag ~= "invoke" or value.fn.tag ~= "var" or value.fn.name ~= "recur" then
            return false
        end

        -- Check if `recur` is invoked anywhere else
        -- (recur ... (recur ...)) is invalid
        for _, v in value.args do
            if contains_var(v, "recur") then
                return false
            end
        end

        -- Arity check
        if #value.args ~= #args then
            errors.arity_err(value.span, "recur", #args, #value.args)
            return false
        end

        -- Replace (recur a b c) with (do (set! a# a) (set! b# b) (set! c# c))
        node.tag = "exec"
        node.exprs = {}

        for i, v in ipairs(value.args) do
            local new_arg_name = gensym(args[i])
            local childnode = ir.setvar(v.span, new_arg_name, v)

            args[i] = new_arg_name
            table.insert(node.exprs, childnode)
        end

        node.value = nil

        table.insert(node.exprs, ir.noop(node.span))
        return true
    else
        -- TODO: check if recur is called inside of lists, hmap and vectors
        return false
    end
end

local function compile_fn(node: any, dynenv: any): string
    local arglist = {}
    for _, name in node.args do
        table.insert(arglist, sanitize_name(z.encode(name)))
    end

    local header = ""
    local args
    if COMPILE_OPTS.arity_check then
        args = "..."
        local arity = #arglist
        
        sourcemap.gensource(node.span)
        if node.varargs then
            header ..= `if select('#', ...) < {arity} then error('fn expects {arity}.. arguments but received ' .. select('#', ...), 2) end\n`
        else
            local count = if arity == 1 then "argument" else "arguments"
            header ..= `if select('#', ...) ~= {arity} then error('fn expects {arity} {count} but received ' .. select('#', ...), 2) end\n`
        end
        
        if arity > 0 then
            sourcemap.gensource(node.span)
            header ..= `local {table.concat(arglist, ", ")} = ...;\n`
        end

        if node.varargs then
            if not dynenv[mangled_list] then
                dynenv[mangled_list] = table_to_list
            end
            
            sourcemap.gensource(node.span)
            if arity > 0 then
                header ..= `local {node.varargs} = {mangled_list}.content(\{select({arity + 1}, ...)}, select('#', ...) - {arity})\n`
            else
                header ..= `local {node.varargs} = {mangled_list}.content(\{ ... }, select('#', ...))\n`
            end
        end
    else
        if node.varargs then
            table.insert(arglist, "...")

            if not dynenv[mangled_list] then
                dynenv[mangled_list] = table_to_list
            end
            
            sourcemap.gensource(node.span)
            header ..= "local " .. node.varargs .. " = " .. mangled_list .. ".content({ ... }, select('#', ...))\n"
        end

        args = table.concat(arglist, ", ")
    end

    local temp_args = table.clone(node.args)
    if node.varargs then
        table.insert(temp_args, node.varargs)
    end
    
    local body = ""
    if COMPILE_OPTS.tailcall_optimize and is_tailcall(node, temp_args) then      
        for i, v in temp_args do
            temp_args[i] = z.encode(v)
        end

        if #node.args > 0 then            
            body = "local " .. table.concat(temp_args, ", ") .. " = " .. table.concat(node.args, ", ")
            if node.varargs then
                body ..= ", " .. node.varargs
            end

            body ..= "; while true do " .. table.concat(node.args, ", ")
            if node.varargs then
                body ..= ", " .. node.varargs
            end

            body ..= " = " .. table.concat(temp_args, ", ") .. "; "
        elseif node.varargs then
            body = "local " .. temp_args[1] .. " = " .. node.varargs
            body ..= "; while true do " .. node.varargs .. " = " .. node.temp_args[1] .. "; "
        end

        body ..= table.concat(compile_exprs(node.body, dynenv), "\n")
        body ..= " end"
    else    
        body = table.concat(compile_exprs(node.body, dynenv), "\n")
    end

    local src = "(function(" .. args .. ") " .. header .. body .. " end)"
    return src
end

local function compile_def(node: any, dynenv: any): string
    local box = node.box
    box.metadata = node.metadata
    
    local mangled = z.encode(box.name)
    if not dynenv[mangled] then
        dynenv[mangled] = box
    end

    if node.value then
        local value = compile_expr(node.value, dynenv)

        -- Set some default metadata based on the var's value
        if node.value.tag == "fn" then
            box.metadata["type"] = "function"
            
            if box.metadata["arglist"] == nil then                    
                local val = node.value
                local arity = tostring(#val.args)
                if val.varargs then
                    arity = arity .. ".."
                end

                box.metadata["arity"] = arity

                local arglist = {}
                for _, arg in val.args do
                    table.insert(arglist, arg)
                end
                
                if val.varargs then
                    table.insert(arglist, "& " .. val.varargs)
                end

                box.metadata["arglist"] = "[" .. table.concat(arglist, " ") .. "]"
            end
        elseif box.metadata["type"]  == nil then
            box.metadata["type"] = "value" 
        end
        
        local src = mangled .. ".content = " .. value
        return src
    else
        sourcemap.gensource(node.span)
        return mangled
    end
end

local function compile_ret(node: any, dynenv: { [string]: any }): string
    local value = compile_expr(node.value, dynenv)
    return "return " .. value
end

local function compile_retvar(node: any, dynenv: { [string]: any }): string
    local value = compile_expr(node.value, dynenv)
    -- sourcemap.gensource(node.span)
    return sanitize_name(node.name) .. " = " .. value
end

local function compile_retdyn(node: any, dynenv: { [string]: any }): string
    local value = compile_expr(node.value, dynenv)
    -- sourcemap.gensource(node.span)
    return z.encode(node.box.name) .. ".content = " .. value
end

function compile_expr(node: any, dynenv: any): string
    local tag = node.tag

    if tag == "const" then
        return compile_const(node, dynenv)
    elseif tag == "vector" then
        return compile_vec(node, dynenv)
    elseif tag == "hmap" then
        return compile_hmap(node, dynenv)
    elseif tag == "list" then
        return compile_list(node, dynenv)
    elseif tag == "var" then
        return compile_var(node, dynenv)
    elseif tag == "dynvar" then
        return compile_dynvar(node, dynenv)
    elseif tag == "native" then
        return compile_native(node, dynenv)
    elseif tag == "ifelse" then
        return compile_ifelse(node, dynenv)
    elseif tag == "let" then
        return compile_let(node, dynenv)
    elseif tag == "invoke" then
        return compile_invoke(node, dynenv)
    elseif tag == "setvar" then
        return compile_setvar(node, dynenv)
    elseif tag == "setfield" then
        return compile_setfield(node, dynenv)
    elseif tag == "def" then
        return compile_def(node, dynenv)
    elseif tag == "fn" then
        return  compile_fn(node, dynenv)
    elseif tag == "exec" then
        return compile_exec(node, dynenv)
    elseif tag == "noop" then
        sourcemap.gensource(node.span)
        return ""
    elseif tag == "op1" then
        return compile_op1(node, dynenv)
    elseif tag == "op2" then
        return compile_op2(node, dynenv)
    elseif tag == "ret" then
        return compile_ret(node, dynenv)
    elseif tag == "retvar" then
        return compile_retvar(node, dynenv)
    elseif tag == "retdyn" then
        return compile_retdyn(node, dynenv)
    elseif tag == "box" then
        return compile_box(node, dynenv)
    end

    error("invalid ir: " .. tostring(tag))
end

local loadstring = loadstring or function(str, name)
    local success, luau = pcall(require, "@lune/luau")
    if not success then
        error("FATAL: could not find function `_G.loadstring`")
    end

    local success, res = pcall(luau.load, str, {
        debugName = name
    })
    
    if not success then
        return nil, res
    end

    return res, nil
end

local i = 1
function compile(node)
    sourcemap.reset()

    local dynenv = {}
    local src = compile_expr(node, dynenv)

    sourcemap.put(node.span.file, i)

    local ns = Namespace.current_ns(true)
    local name = `CRES$chunk;{i};{ns};`

    local prelude = {}
    local vals = {}
    for k, v in dynenv do
        table.insert(prelude, k)
        table.insert(vals, v)
    end

    local compiled = "return function() " .. src .. " end"
    if #prelude > 0 then
        compiled = "local " .. table.concat(prelude, ", ") .. " = ...; " .. compiled
    end

    local fn, err = loadstring(compiled, name)
    i += 1
    if fn == nil then
        print(src)
        error("error during compilation: " .. tostring(err))
    end

    return fn(unpack(vals))
end

return compile