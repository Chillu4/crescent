--!strict
local typeof = require("typeof")

local HMAP_MT = {}

function HMAP_MT:__tostring()
    local acc = {}
    for k, v in pairs(self) do
        table.insert(acc, tostring(k) .. " " .. tostring(v))
    end

    return "{" .. table.concat(acc, " ") .. "}"
end

function HMAP_MT:__len()
    local length = 0
    for _, _ in pairs(self) do
        length += 1
    end

    return length
end

function HMAP_MT:__call(index)
    if type(index) ~= "nil" then
        return self[index]
    end

    error("attempt to call a hmap with a nil value", 2)
end

function HMAP_MT:__eq(other)
    if rawequal(self, other) then
        return true
    end

    if typeof(other) == "hmap" then
        local self_keys = {}
        for k, _ in pairs(self) do
            self_keys[k] = true
        end

        local checked_count = 0
        for k, v in pairs(other) do
            local self_key = self_keys[k]
            if self_key == nil or self[k] ~= other[k] then
                return false
            end

            checked_count += 1
        end

        return checked_count == #self_keys
    end

    return false
end

HMAP_MT.__metatable = { __type = "hmap" }

local function from_table(tbl)
    return setmetatable(tbl, HMAP_MT)
end

local function hmap(...)
    if select("#", ...) % 2 ~= 0 then
        error("argument must be of even number")
    end

    local tbl = {}
    for i = 2, select("#", ...), 2 do
        tbl[select(i - 1, ...)] = select(i, ...)
    end

    return from_table(tbl)
end

return {
    hmap = hmap,
    from_table = from_table
}