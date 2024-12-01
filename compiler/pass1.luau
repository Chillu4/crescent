--!strict
local ir = require("ir")
local gensym = require("gensym")

local function replace_tail(node: any, cb: (any) -> any)
    local tag = node.tag
    if tag == "fn" then
        node.body[#node.body] = replace_tail(node.body[#node.body], cb)
        return node
    elseif tag == "let" then
        node.body[#node.body] = replace_tail(node.body[#node.body], cb)
        return node
    elseif tag == "exec" then
        node.exprs[#node.exprs] = replace_tail(node.exprs[#node.exprs], cb)
        return node
    elseif tag == "ifelse" then
        node.consq = replace_tail(node.consq, cb)
        node.alt = replace_tail(node.alt, cb)
        return node
    elseif tag == "setvar" then
        local new_value = cb(ir.var(node.name))
        return ir.exec({ node, new_value })
    elseif tag == "def" then
        local new_value = cb(
            ir.dynenv(
                node.namespace,
                node.name,
                node.box
            )
        )
        return ir.exec({ node, new_value })
    elseif tag == "setfield" then
        local sym = gensym()
        local new_value = cb(ir.var(sym))
        return ir.let(false, {{sym, node}}, { new_value })
    elseif tag == "ret" or tag == "retvar" then
        return cb(node.value)
    else
        return cb(node)
    end
end

local function is_expr(node: any)
    local exprs = {
        ["const"] = true,
        ["dynvar"] = true,
        ["op1"] = true,
        ["op2"] = true,
        ["hmap"] = true,
        ["list"] = true,
        ["vector"] = true,
        -- ["fn"] = true
    }

    return exprs[node.tag] ~= nil
end

local function is_stmt(node: any)
    local stmts = {
        ["ifelse"] = true,
        ["setvar"] = true,
        ["def"] = true,
        ["let"] = true,
        ["exec"] = true,
        ["ret"] = true,
    }

    return stmts[node.tag] ~= nil
end

local function identity(node) 
    return node
end

local function cc_to_stmt(node): any
    if is_expr(node) then
        return ir.setvar(node.span, "_", node)
    end

    -- (set! symbol <expr>)
    -- (def <symbol> <expr>)
    if node.tag == "setvar" then
        return ir.exec(node.span, { table.clone(node), ir.var(node.span, node.name) })
    elseif node.tag == "def" then
        return ir.exec(node.span, { table.clone(node), ir.dynvar(node.span, node.namespace, node.name), node.box })
    end
    return node
end

local function cc_return(node)
    return ir.ret(node.span, node)
end

local function iife(node)
    node = replace_tail(node, cc_return)
    return ir.invoke(node.span, ir.fn(node.span, {}, nil, { node }), {})
end

function l2(node: any, cc: (any) -> any)
    local tag = node.tag
    if tag == "const" or tag == "var" or tag == "dynvar" or tag == "native" or tag == "box" then
        local new_node = cc(node)
        -- require("../pprint")(node)
        return new_node

    elseif tag == "setvar" then
        node.value = l2(node.value, identity)
        local new_node = cc(ir.const(node.span, "nil", nil))
        
        if is_stmt(node.value) then
            local name = node.name
            node = l2(node.value, function(node)
                return ir.setvar(node.span, name, node)
            end)
        end

        return ir.exec(node.span, { node, new_node })

    elseif tag == "setfield" then
        local binds = {}
        node.tbl = l2(node.tbl, identity)

        if is_stmt(node.tbl) then
            local name = gensym()
            local cc_retvar = function(node)
                return ir.retvar(node.span, name, node)
            end
            node.tbl = ir.var(node.tbl.span, name)
            table.insert(binds, {name, l2(node.tbl, cc_retvar)})
        end

        node.field = l2(node.field, identity)
        if is_stmt(node.field) then
            local name = gensym()
            local cc_retvar = function(node)
                return ir.retvar(node.span, name, node)
            end
            node.field = ir.var(node.field.span, name)
            table.insert(binds, {name, l2(node.field, cc_retvar)})
        end

        node.value = l2(node.value, identity)
        if is_stmt(node.value) then
            local name = gensym()
            local cc_retvar = function(node)
                return ir.retvar(node.span, name, node)
            end

            node.value = ir.var(node.value.span, name)
            table.insert(binds, {name, l2(node.value, cc_retvar)})
        end

        local new_node = cc(ir.const(node.span, "nil", nil))
        node = ir.exec(node.span, { table.clone(node), new_node })

        if #binds > 0 then
            return ir.let(node.span, false, binds, { node })
        end

        return node
    elseif tag == "ifelse" then
        node.consq = l2(node.consq, cc)
        node.alt = l2(node.alt, cc)

        if is_stmt(node.cond) then
            local cond = node.cond
            
            local sym = gensym()
            node.cond = ir.var(node.cond.span, sym)
            
            local cc = function(node)
                return ir.setvar(node.span, sym, cond)
            end

            return ir.let(node.span, false, {{sym, l2(cond, cc)}}, { node })
        end

        return node
    elseif tag == "exec" then
        local exprs = node.exprs
        for i = 1, #exprs - 1 do
            exprs[i] = l2(exprs[i], cc_to_stmt)
        end

        if #exprs > 0 then
            local last = exprs[#exprs]
            exprs[#exprs] = l2(last, cc)
        end

        return node

    elseif tag == "fn" then
        local body = node.body
        for i = 1, #body - 1 do
            body[i] = l2(body[i], cc_to_stmt)
        end

        if #body > 0 then
            local last = body[#body]
            body[#body] = l2(last, cc_return)
        end

        return cc(node)
    elseif tag == "noop" then
        return node
    elseif tag == "op1" then
        local left = l2(node.left, identity)
        
        if is_stmt(left) then
            local sym = gensym()
            node.left = ir.var(left.span, sym)
            
            left = l2(left, function(node)
                return ir.retvar(node.span, sym, node)
            end)

            return ir.let(node.span, false, {{sym, left}}, { node })
        end
        
        return cc(node)

    elseif tag == "op2" then
        local left = l2(node.left, identity)
        local right = l2(node.right, identity)

        local binds = {}
        if is_stmt(left) then
            local sym = gensym()
            node.left = ir.var(left.span, sym)
            
            left = l2(left, function(node)
                return ir.retvar(node.span, sym, node)
            end)

            table.insert(binds, {sym, left})
        end

        if is_stmt(right) then
            local sym = gensym()
            node.right = ir.var(right.span, sym)
            
            right = l2(right, function(node)
                return ir.retvar(node.span, sym, node)
            end)

            table.insert(binds, {sym, right})
        end

        node = cc(node)

        if #binds > 0 then
            return ir.let(node.span, false, binds, { node })
        end

        return node
    elseif tag == "let" then
        local body = node.body
        for _, bind in node.binds do
            local name = bind[1]
            local value = l2(bind[2], identity)
            if is_stmt(value) then
               bind[2] = iife(value)
            else
                bind[2] = value
            end
        end

        for i = 1, #body - 1 do
            body[i] = l2(body[i], cc_to_stmt)
        end

        if #body > 0 then
            local last = body[#body]
            body[#body] = l2(last, cc)
        end

        return node
    elseif tag == "list" or tag == "vector" then
        local elements = node.elements
        for i = 1, node.length do
            local element = l2(elements[i], identity)
            if is_stmt(element) then
                elements[i] = iife(element)
            else
                elements[i] = element
            end
        end

        return cc(node)

    elseif tag == "hmap" then
        local acc = {}
        for k, v in node.elements do
            local key = l2(k, identity)
            if is_stmt(key) then
                key = iife(key)
            end

            local value = l2(v, identity)
            if is_stmt(value) then
                value = iife(value)            
            end

            acc[key] = value
        end

        node.elements = acc
        return cc(node)
    elseif tag == "invoke" then        
        node.fn = l2(node.fn, identity)
        if is_stmt(node.fn) then
            node.fn = iife(node.fn)
        end

        local args = node.args
        for i = 1, #args do
            local arg = l2(args[i], identity)
            if is_stmt(arg) then
                args[i] = iife(arg)
            else
                args[i] = arg
            end
        end

        return cc(node)
    elseif tag == "def" then
        node.value = l2(node.value, identity)
        local span = node.span
        local namespace = node.namespace
        local name = node.name
        local box = node.box

        if is_stmt(node.value) then
            -- local name = gensym()
            local cc_retdyn = function(node)
                return ir.retdyn(node.span, box, node)            
            end
            
            node = replace_tail(node.value, cc_retdyn)
        end

        return ir.exec(span, { node, cc(ir.box(span, namespace, name, box)) })
    else
        return node
    end
end

return function(node)
    return l2(node, cc_return)
end
