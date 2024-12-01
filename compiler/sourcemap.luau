--!strict
local SOURCE_REGISTRY = {}

local LINE_NO = 1
local temp_sources = {}

local function gensource(span)
    temp_sources[LINE_NO] = span
    LINE_NO += 1
end

local function reset()
    temp_sources = {}
    LINE_NO = 1
end

local function put(filename: string, id: number)
    temp_sources.filename = filename
    SOURCE_REGISTRY[id] = temp_sources
    reset()
end

local function get(id: number)
    return SOURCE_REGISTRY[id]
end

return {
    gensource = gensource,
    reset = reset,
    put = put,
    get = get,
}