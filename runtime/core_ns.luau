--!strict
local Namespace = require("namespace")
local typeof = require("typeof")
local vector = require("vector")
local hmap = require("hmap")
local list = require("list")
local keyword = require("keyword")
local sym = require("sym")
local gensym = require("../compiler/gensym")
local errors = require("../compiler/errors/errors")
local lazy_cons = require("lazy_cons")
local reader = require("../reader")

local core = Namespace.new("cres.core")
Namespace.set_core(core)

Namespace.new("default")
core:intern(
    "*ns*",
    {doc = "Contains the namespace being currently used"}, 
    "default"
)

core:intern(
    "read-string",
    {},
    function(str)
        return reader.read("<read-string>", str)
    end
)

core:intern(
    "doc",
    {doc = "Returns the documentation associated with a given var"},
    function(symbol): string
        local symname = tostring(symbol)
        local namespace, name = string.match(symname, "([^/]*)/(.*)")
        if namespace == nil then
            name = symname
        end

        local box = Namespace.current_ns():get(namespace, name :: string)
        if not box then
            error("could not resolve " .. symname)
        end

        local prelude = "-- doc --\n" .. box.name
        if box.metadata.type == "function" or box.metadata.type == "macro" then
            prelude ..= "\n" .. tostring(box.metadata.args or box.metadata.arglist) .. "\n" .. box.metadata.type .. "\n\t"
        elseif box.metadata.type == "special" then
            prelude ..= "\n" .. tostring(box.metadata.args) .. "\nSpecial form\n\t"
        else
            prelude ..= "\n" .. box.metadata.type .. "\n\t"
        end

        local body = box.metadata.doc or "No documentation available."
        return prelude .. body
    end
)

core:intern(
    "table",
    {},
    function(...)
        if select("#", ...) % 2 ~= 0 then
            error("expected even number of arguments")
        end

        local tbl = {}
        for i = 1, select("#", ...), 2 do
            tbl[i] = select(i + 1, ...)
        end
        
        return tbl
    end
)

core:intern(
    "vec",
    {doc = "Converts a given collection into a vector"},
    vector.vec
)

core:intern(
    "table->vec",
    {doc = "Converts a Lua table into a vector"},
    vector.from_table
)

core:intern(
    "hmap",
    {doc = "Converts a given collection into a hmap"},
   hmap.hmap
)

core:intern(
    "table->hmap",
    {doc = "Converts a given Lua table into a hmap"},
   hmap.from_table
)

core:intern(
    "list",
    {},
    list.list
)

core:intern(
    "cons",
    {},
    list.cons
)

core:intern(
    "table->list",
    {doc = "Converts a Lua table into a list"},
    list.from_table
)

core:intern(
    "keyword",
    {},
    keyword
)

core:intern(
    "sym",
    {},
    sym
)

core:intern(
    "type",
    {},
    function(x)
        return keyword(typeof(x))
    end
)

core:intern(
    "apply",
    {},
    function(fn, ...)
        local vargs = table.pack(...)
        if vargs.n == 0 then
            return fn()
        end

        local seq = table.remove(vargs, vargs.n)
        vargs.n -= 1

        local length = 0
        if typeof(seq) == "list" then
            local current = seq
            local i = 1

            while current ~= list.EMPTY_LIST do
                vargs[vargs.n + i] = current.head
                current = current.tail
                i += 1
            end

            length = i - 1

        elseif type(seq) == "table" then
            for i = 1, seq.n do
                vargs[vargs.n + i] = seq[i]
            end
            
            length = seq.n
        else
            error("last argument should be a seq")
        end

        return fn(unpack(vargs, 1, vargs.n + length))
    end
)

core:intern(
    "gensym",
    {},
    function(prefix)
        return sym(gensym(prefix))
    end
)

core:intern(
    "name",
    {},
    function(x)
        return rawget(x, "__name")
    end
)

core:intern(
    "make-error*",
    { hidden = true },
    function(x)
        local err = {
            message = x.message,
            type = x.type
        }

        setmetatable(err, errors.CRES_ERR_MT)
        return err
    end
)

core:intern("lazy-cons", {}, lazy_cons)

core:intern("import*", {},
    function(...)
        local skip_arg = false
        for i = 1, select("#", ...) do
            if skip_arg then
                skip_arg = false
                continue
            end

            local ns_sym = select(i, ...)
            if typeof(ns_sym) ~= "symbol" then
                error("expected symbol value for argument #" .. i .. ", but got " .. tostring(typeof(ns_sym)), 2)
            end

            -- Check if it's followed by an hmap
            local next_arg = select(i + 1, ...)
            local options = {}
            if typeof(next_arg) == "hmap" then
                -- Parse options
                options["except"] = next_arg[keyword("except")]
                options["as"] = next_arg[keyword("as")]
                options["expose"] = next_arg[keyword("expose")]

                skip_arg = true
            end

            local ns_to_import = Namespace.get_ns(rawget(ns_sym, "__name"))
            local current_ns = Namespace.current_ns()

            current_ns:import(ns_to_import, rawget(options.as, "__name"))
            if options.expose then
                local err_msg = `option ':expose' should accept a vector of symbols`
                if typeof(options.expose) ~= "vector" then
                    error(err_msg, 2)
                end

                for _, symbol in ipairs(options.expose) do
                    if typeof(symbol) ~= "symbol" then
                        error(err_msg, 2)
                    end

                    local sym_name = rawget(symbol, "__name")
                    local box = ns_to_import:get("", sym_name)
                    if not box then
                        error(`could not expose {sym_name} as it does not exist in namespace {rawget(ns_sym, "__name")}`, 2)
                    end

                    current_ns:map(sym_name, box)
                end
            end
        end
    end
)

return core