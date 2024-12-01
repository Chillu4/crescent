--!strict
local VEC_MT = {}

function VEC_MT:__newindex(index: number, value: any): nil
    if type(index) ~= "number" then
        error("index of vector must be a number")
    end

    if index > #self + 1 then
        error("vector assignment index out of range")
    end
    
    rawset(self, index, value)
    return nil
end

function VEC_MT:__index(index: number): any
    if type(index) ~= "number" then
        error("vector can only be indexed with number")
    end

    return rawget(self, index)
end

function VEC_MT:__eq(other: any): boolean
    if rawequal(self, other) then
        return true
    end
    
    if type(other) == "table" and getmetatable(other) == VEC_MT then
        local len_self = #self
        local len_other = #other
        
        if len_self ~= len_other then
            return false
        end

        for i = 1, len_self do
            local val_self = rawget(self, i)
            local val_other = rawget(other, i)

            if val_self ~= val_other then
                return false
            end
        end

        return true
    end

    return false
end

function VEC_MT:__call(index: number): any
    if type(index) == "number" then
        return rawget(self, index)
    end

    error("attempt to call a vector with a non-numeric value", 2)
end

function VEC_MT:__tostring()
    local str = {}
    for i = 1, #self do
        table.insert(str, tostring(self[i]))
    end

    return "[" .. table.concat(str, " ") .. "]"
end

VEC_MT.__metatable = { __type = "vector" }

local function from_table(tbl: { n: number?, [number]: any }, n: number?): Vector
    tbl.n = n or tbl.n or #tbl
    return setmetatable(tbl, VEC_MT)
end

local function vec(...): Vector
    local elements = table.pack(...)
    return from_table(elements, elements.n)
end

export type Vector = typeof(vec({}, 0))

return {
    vec = vec,
    from_table = from_table
}