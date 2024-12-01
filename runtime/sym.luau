local typeof = require("typeof")

local SYMBOLS = {}

local SYM_MT = {}
function SYM_MT:__tostring()
    return rawget(self, "__name")
end

function SYM_MT:__index(index)
    error("attempt to index symbol with '" .. index .. "'")
end

function SYM_MT:__newindex(index)
    error("attempt to index symbol with '" .. index .. "'")
end

function SYM_MT:__len()
    error("attempt to get length of a symbol value")
end

function SYM_MT:__call()
    error("attempt to call a symbol value (" .. rawget(self, "__name") .. ")", 2)
end

function SYM_MT:__eq(other)
    return rawequal(self, other) or (
            typeof(other) == "symbol" 
            and rawget(self, "__name") == rawget(other, "__name")
        )
end

SYM_MT.__metatable = { __type = "symbol" }

local function sym(name: string)
    -- if not SYMBOLS[name] then
    --     local symbol = setmetatable({ __name = name }, SYM_MT)
    --     SYMBOLS[name] = symbol
    -- end

    -- return SYMBOLS[name]
    return setmetatable({ __name = name }, SYM_MT)
end

return sym