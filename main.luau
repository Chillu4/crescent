--!strict
require("runtime/core_ns")

local reader = require("reader")

local lower = require("compiler/lower")
local lower2 = require("compiler/pass1")

local compile = require("compiler/compile")
local error_handler = require("compiler/errors/format")

local function eval(ast)
    local success, res = pcall(function()
        local ir = lower({}, ast)
        return lower2(ir)
    end)

    if not success then
        if type(res) == "string" then
            error("FATAL ERROR: " .. res)
        end

        error(`{res.error}{res.span}: {res.message}`, 0)
    end

    local fn = compile(res)

    local success, result = xpcall(fn, error_handler.traceback)
    if not success then
        error(error_handler.format_traceback(result), 0)
    end

    return result
end

local function multieval(src: string)
    local res = reader.read_until_eof('REPL', src)
    local out = { n = 0 }

    for _, ast in res do
        local success, res = pcall(eval, ast)
        
        out[out.n + 1] = if success then res else error(res)
        out.n += 1
    end

    return out
end

return {
    eval = eval,
    multieval = multieval
}