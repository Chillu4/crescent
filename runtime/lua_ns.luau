--!strict
local Namespace = require("namespace")

local op_ns = Namespace.new("lua.op")
op_ns:intern(
    "+",
    {
        doc = "Returns the sum of two numbers by performing Lua's addition. IF USED AS",
        const = true
    }
)

local _fenv_ns = Namespace.new("lua.fenv")