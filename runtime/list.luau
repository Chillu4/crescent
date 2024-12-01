--!strict
local typeof = require("typeof")

local LIST_MT = {}
local EMPTY_LIST = setmetatable({}, LIST_MT)

function LIST_MT:__tostring()
    if rawequal(self, EMPTY_LIST) then
        return "()"
    end

    local acc = {}
    local current = self
    while not rawequal(current, EMPTY_LIST) do
        table.insert(acc, tostring(rawget(current, "head")))
        current = rawget(current, "tail")
    end

    return "(" .. table.concat(acc, " ") .. ")"
end

function LIST_MT:__index(index)
    if index == "tail" or index == "head" then
        return rawget(self, index)    
    end

    if type(index) == "number" then
        local i = 1
        local current = self
        while i <= index and current ~= EMPTY_LIST do
            current = rawget(self, "tail")
            i += 1
        end

        if current == nil then
            error("index out of bounds for index " .. index)
        end

        return rawget(self, "head")
    end

    error("attempt to index list with '" .. tostring(index) .. "'")
end

function LIST_MT:__newindex(index)
    error("attempt to set list with index '" .. tostring(index) .. "'")
end

function LIST_MT:__call()
    error("attempt to call a list value", 2)
end

function LIST_MT:__len()
    if rawequal(self, EMPTY_LIST) then
        return 0
    end
    
    local current = self
    local length = 0
    while current ~= EMPTY_LIST do
        current = rawget(current, "tail")
        length += 1
    end

    return length
end

function LIST_MT:__eq(other)
    if rawequal(self, other) then
        return true
    end

    if type(other) ~= "table" or getmetatable(other) ~= LIST_MT then
        return false
    end

    local self_current = self
    local other_current = other
    while self_current ~= nil and other_current ~= nil do
        if rawget(self_current, "head") ~= rawget(other_current, "head") then
            return false
        end

        self_current = rawget(self_current, "tail")
        other_current = rawget(other_current, "tail")
    end

    return self_current ~= nil and other_current ~= nil
end

LIST_MT.__metatable = { __type = "list" }

local function cons1(element: any, list)
    local type = typeof(list)
    if type == "nil" then
        list = EMPTY_LIST
    elseif type ~= "list" then
        error("expected argument #2 to be type list, but got " .. type)    
    end

    return setmetatable({
        head = element,
        tail = list
    }, LIST_MT)
end

local function from_table(elements, n)
    local current = EMPTY_LIST
    for i = n, 1, -1 do
        current = cons1(elements[i], current)
    end

    return current
end

local function list(...: any)
    local elements = table.pack(...)
    return from_table(elements, elements.n)
end

local function cons(element: any, coll: any)
    local type = typeof(coll)
    if type == "vector" then
        coll = from_table(coll, #coll)
    end

    return cons1(element, coll)
end

export type List = {
    head: any,
    tail: List
} | {typeof(EMPTY_LIST)}

return {
    EMPTY_LIST = EMPTY_LIST,
    cons = cons,
    list = list,
    from_table = from_table
}