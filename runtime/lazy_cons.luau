--!strict
local LAZY_CONS_MT = {}

function LAZY_CONS_MT.__index(self, index)
    if index ~= "head" and index ~= "tail" then
        error(`attempt to access index {index} of lazy-cons`, 2)
    end
 
    local property = "__" .. index .. "_thunk"
    local result = rawget(self, property)()
    rawset(self, property, nil)
    rawset(self, index, result)
    return result
end

function LAZY_CONS_MT.__newindex()
    error("attempt to set index of lazy-cons", 2)
end

function LAZY_CONS_MT.__tostring()
    return "#<lazy-cons>"
end

LAZY_CONS_MT.__type = "lazy-cons"

-- Constructor
local function lazy_cons(head_thunk, tail_thunk)
    if type(head_thunk) ~= "function" then
        error("argument #1 should be a function, got type " .. tostring(head_thunk))
    end
        
    if type(tail_thunk) ~= "function" then
        error("argument #2 should be a function, got type " .. tostring(head_thunk))
    end

    return setmetatable({
        __head_thunk = head_thunk,
        __tail_thunk = tail_thunk,
    }, LAZY_CONS_MT)
end

return lazy_cons