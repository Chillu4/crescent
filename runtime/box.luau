--!strict

local VAR_MT = {
    __call = function(self: Box, ...: any): any
        return self.content(...)
    end,

    __tostring = function(self: Box): string
        return "#'" .. self.name
    end
}

local function box(name: string, metadata: { [string]: any }, value: any): Box
    local self = {
        name = name,
        content = value,
        metadata = metadata,
    }

    return setmetatable(self, VAR_MT)
end

export type Box = typeof(box("", {}, nil))

return box