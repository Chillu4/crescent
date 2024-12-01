--!strict
local fs = require("@lune/fs")
local stdio = require("@lune/stdio")
local main = require("main")

local src = fs.readFile("stdlib/core.clj")
local out = main.multieval(src)

local Namespace = require("./runtime/namespace")

while true do
    local text = stdio.prompt("text", `{Namespace.current_ns(true)}=>`)
    print(main.multieval(text)[1])
end