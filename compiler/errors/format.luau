--!strict
local sourcemap = require("../sourcemap")

local function lines(str)
    local lines = {}
    for line in string.gmatch(str, "([^\n]*)\n?") do
        table.insert(lines, line)
    end

    return lines
end

local function cres_fn(name: string): (number?, string)
    local substrings = string.split(name, ";")
    if substrings[1] ~= '[string "CRES$chunk' or #substrings ~= 4 then
        return nil, ""
    end

    return tonumber(substrings[2]), substrings[3]
end

local function traceback(obj)
    local stacktrace = {}
    print(obj)
    for level = 2, math.huge do
        local source, line, name = debug.info(level, "sln")
        if source == nil then
            break
        end

        local id = cres_fn(source)
        local current_level: any
        if source == "[C]" then
            current_level = {
                type = "C",
                name = name
            }
        elseif id ~= nil then
            local src_info = sourcemap.get(id)[line]
            current_level = {
                type = "crescent",
                span = src_info.span,
                file = src_info.file
            }
        else
            current_level = {
                type = "lua",
                source = source,
                line = line,
                name = name
            }
        end

        table.insert(stacktrace, current_level)
    end


    if type(obj) == "string" then
        local file, line, msg = string.match(obj, '^(.-):(%d+): (.*)$')
        if file and line and msg then
            obj = "RuntimeError: " .. msg
        end
    end

    return {
        obj = obj,
        stacktrace = stacktrace
    }
end

local function format_traceback(error)
    local header = tostring(error.obj)
    local stacktrace = { "stacktrace:" }

    for _, info in error.stacktrace do
        local str
        if info.type == "lua" then
            str = `[  Lua   ] {info.source}:{info.line}: {if info.name then "function " .. info.name else ""}`
        elseif info.type == "C" then
            str = `[   C    ] function {info.name}`
        elseif info.type == "crescent" then
            str = `[Crescent] {info.file or "<unknown>"} {info.span}`
        end

        table.insert(stacktrace, str)
    end

    return `{header}\n{table.concat(stacktrace, "\n")}`
end

return {
    format_traceback = format_traceback,
    traceback = traceback,
    lines = lines,
}