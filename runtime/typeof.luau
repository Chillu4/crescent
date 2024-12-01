--!strict

-- Extended version of the in-build `type` function in Lua.
-- If the given argument is a table, and contains a metatable with the field "__type", then
--  that field is used as the table's type, otherwise defaults to "table".
local function typeof(expr)
    local t = type(expr)
    if t == "table" then
        local mt = getmetatable(expr)
        if mt then
            return rawget(mt, "__type") or "table"
        end
    end

    return t
end

return typeof