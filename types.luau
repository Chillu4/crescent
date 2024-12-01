--!strict
local exports = {}

-- Metatable
local STX_MT = {
    __tostring = function(self)
        return tostring(self.datum)
    end
}

local POSITION_MT = {
    __tostring = function(self)
        return tostring(self.line) .. ":" .. tostring(self.column)
    end
}

local SPAN_MT = {
    __tostring = function(self)
        return `[{self.from} .. {self.to}]`
    end
}

-- Constructor
function exports.Span(pos1: Position, pos2: Position): Span
    return setmetatable({
        from = pos1,
        to = pos2
    }, SPAN_MT)
end

function exports.Position(line: number, column: number, index: number): Position
    return setmetatable({
        line = line,
        column = column,
        index = index
    }, POSITION_MT)
end

function exports.SyntaxObject<T>(datum: T, span: Span, file: string): SyntaxObject<T>
    return setmetatable({
        datum = datum,
        span = span,
        file = file
    }, STX_MT)
end

-- Single instances
exports.empty_span = exports.Span(exports.Position(0, 0, 0), exports.Position(0, 0, 0))
exports.EOF = table.freeze({})

-- Types
export type Position = typeof(exports.Position(0, 0, 0))
export type Span = typeof(exports.empty_span)
export type SyntaxObject<T> = typeof(setmetatable({} :: {
    datum: T,
    span: Span,
    file: string
}, {} :: typeof(STX_MT)))
export type EOF = typeof(exports.EOF)

return exports