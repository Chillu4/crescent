--!strict
local types = require("../../types")

type Span = types.Span

local function err(err: string, span: Span, message: string): never
    local kind = {
        type = err,
        message = message,
        span = span,
    }

    setmetatable(kind, { __type = `error/{type}`, __crescent_error = true })
    print(err, message, span)
    error(kind)
end

local function rt_err(span, message: string): never
    err("RuntimeError", span, message)
    error("")
end

local function type_err(span, arg: number, expected: string, received: string): never
    err("TypeError", span, `expected {expected} for argument #{arg} but got {received}`)
    error("")
end

local function arity_err(span, name: string, expected: number | string, given: number | string): never
    local word1 = "argument"
    if expected ~= 1 then
        word1 = word1 .. "s"
    end

    local word2 = "argument"
    if given ~= 1 then
        word2 = word2 .. "s"
    end

    err("ArityError", span, "fn '" .. name .. "' expects " .. expected .. " " .. word1 .. " but received " .. given)
    error("")
end

local function compile_err(span, message: string): never
    err("CompileError", span, message)
    error("")
end

local function namespace_err(span, message: string): never
    err("NamespaceError", span, message)
    error("")
end

local function resolve_err(span, name: string, current: string): never
    err("NamespaceError", span, `could not resolve symbol '{name}' in namespace {current}`)
    error("")
end

return {
    compile_err = compile_err,
    arity_err = arity_err,
    type_err = type_err,
    rt_err = rt_err,
    namespace_err = namespace_err,
    resolve_err = resolve_err,
    err = err,
}