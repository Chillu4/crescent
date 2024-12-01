--!strict
local box = require("box")
-- local errors = require("../error")

type Box = box.Box

-- local compile_err = errors.compile_err

local NAMESPACES: { [string]: Namespace } = {}
local CORE_NS: Namespace? = nil

local _PROTECTED_NS = {
    ["lua.op"] = true,
    ["lua.fenv"] = true,
}

local Namespace = {}
Namespace.__index = Namespace

-- Static methods
function Namespace.new(name: string): Namespace
    local self = {
        name = name,
        imports = { ["cres.core"] = CORE_NS } :: { [string]: Namespace },
        mappings = {} :: { [string]: Box },
        fallback = { CORE_NS } :: { Namespace },
        vars = {} :: { [string]: Box },
    }

    setmetatable(self, Namespace)
    NAMESPACES[name] = self

    return self
end

function Namespace.get_ns(ns: string): Namespace
    local v = NAMESPACES[ns]
    if not v then
        error(`namespace '{ns}' does not exist`)
    end

    return v
end

function Namespace.current_ns(name_only: boolean?): Namespace
    local core = Namespace.get_ns("cres.core")
    local ns = core:get("cres.core", "*ns*")
    if not ns then
        error("could not get cres.core/*ns*")    
    end

    if name_only then
        return ns.content
    end

    return NAMESPACES[ns.content]
end

-- Instance methods
function Namespace:get(namespace: string?, name: string): Box?
    local value: Box

    if namespace == self.name then
        value = self.vars[name]
        if not value then
            for _, fallback in self.fallback do
                value = fallback.vars[name]
                if value then
                    return value
                end
            end
        end
    elseif namespace ~= nil then
        local ns = self.imports[namespace]
        if not ns then
            return nil
        end

        value = ns.vars[name]
    else
        value = self.vars[name] or self.mappings[name]
    end

    return value
end

function Namespace:resolve(name: string): (string?, string?)
    if self.vars[name] then
        return self.name, name
    end

    local var = self.mappings[name]
    if self.mappings[name] then
        return string.match(var.name, "([^/]*)/(.*)")
    end

    for _, fallback in self.fallback do
        if fallback.vars[name] then
            return fallback.name, name
        end
    end

    -- error(`could not resolve symbol '{name}' in ns '{self.name}'`)
    return nil
end

function Namespace:intern(name: string, metadata: { [string]: any }?, initial_value: any?): Box
    local value = self.vars[name]
    if not value then
        value = box(`{self.name}/{name}`, metadata or {}, initial_value)
        self.vars[name] = value
    else
        value.metadata = metadata
    end

    return value
end

function Namespace:import(ns: Namespace, alias: string?)
    self.imports[alias or ns.name] = ns
end

function Namespace:map(name: string, box: Box)
    self.mappings[name] = box
end

function Namespace.set_core(core_ns: Namespace)
    CORE_NS = core_ns

    for _, ns in NAMESPACES do
        table.insert(ns.fallback, core_ns)
    end
end

-- Setup protected namespaces 'lua.fenv' and 'lua.op'
-- Protected namespaces are read-only namespaces, i.e vars cannot be created or re-defined.

export type Namespace = typeof(Namespace.new(""))

return Namespace