local typeof = require("typeof")

local KEYWORDS = {}

local KW_MT = {}
function KW_MT:__tostring()
    return ":" .. rawget(self, "__name")
end

function KW_MT:__index(index)
    error("attempt to index keyword with '" .. index .. "'")
end

function KW_MT:__newindex(index)
    error("attempt to index keyword with '" .. index .. "'")
end

function KW_MT:__len()
    error("attempt to get length of a keyword value")
end

function KW_MT:__call(other)
    if typeof(other) == "hmap" or typeof(other) == "table" then
        return other[self]
    end

    error("attempt to call a keyword with a non-hmap value", 2)
end

function KW_MT:__eq(other)
    return rawequal(self, other) or (
            typeof(other) == "keyword" 
            and rawget(self, "__name") == rawget(other, "__name")
        )
end

KW_MT.__metatable = { __type = "keyword" }

local function keyword(name: string)
    assert(type(name) == "string")

    if not KEYWORDS[name] then
        local kw = setmetatable({ __name = name }, KW_MT)
        KEYWORDS[name] = kw
    end
    
    return KEYWORDS[name]
end

return keyword