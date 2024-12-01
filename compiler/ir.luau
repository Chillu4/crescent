--!strict
local ir = {}

-- Helper function to make an IR constructor
local function make_ir(name, props: { string })
    ir[name] = function(span, ...)
       local tmp = { tag = name, span = span }
        for i, prop in props do
            tmp[prop] = select(i, ...)
        end

        return tmp
    end
end 

make_ir("const", {"type", "value"})
make_ir("var", {"name"})
make_ir("dynvar", {"namespace", "name", "box"})
make_ir("box", {"namespace", "name", "box"})
make_ir("ifelse", {"cond", "consq", "alt"})
make_ir("fn", {"args", "varargs", "body"})
make_ir("let", {"recursive", "binds", "body"})
make_ir("invoke", {"fn", "args"})
make_ir("setvar", {"name", "value"})
make_ir("def", {"namespace", "name", "value", "metadata", "box"})
make_ir("exec", {"exprs"})
make_ir("vector", {"elements", "length"})
make_ir("hmap", {"elements"})
make_ir("list", {"elements", "length"})
make_ir("noop", {})
make_ir("native", {"value"})
make_ir("op1", {"op", "left"})
make_ir("op2", {"op", "left", "right"})
make_ir("setfield", {"tbl", "field", "value"})
make_ir("ret", {"value"})
make_ir("retvar", {"name", "value"})
make_ir("retdyn", {"box", "value"})

return ir