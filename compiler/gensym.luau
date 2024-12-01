--!strict
local GS_COUNT = 0

local function gensym(prefix: string?): string
    GS_COUNT += 1
    return (prefix or "") .. "__GENSYM_" .. GS_COUNT
end

return gensym