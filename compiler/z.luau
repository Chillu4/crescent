-- Returns a new table where the values will be the key in the new table, and its corresponding key will be its value. 
-- The keys and values in the old table are "flipped".
-- Example: Flipping { k = v, x = y } will return { v = k, y = x }
local function flip_table(tbl)
    local flipped = {}
    for k, v in pairs(tbl) do
        flipped[v] = k
    end

    return flipped
end

-- Contains a mapping of symbols to its corresponding Z encoded character.
local ENCODE_MAP = {
    ['`'] = 'a',
    ['~'] = 'b',
    ['!'] = 'c',
    ['@'] = 'd',
    ['#'] = 'e',
    ['$'] = 'f',
    ['%'] = 'g',
    ['^'] = 'h',
    ['&'] = 'i',
    ['*'] = 'j',
    ['-'] = 'k',
    ['_'] = 'l',
    ['='] = 'm',
    ['+'] = 'n',
    ['\\'] = 'o',
    ['|'] = 'p',
    [':'] = 'q',
    ['z'] = 'r',
    [','] = 's',
    ['<'] = 't',
    ['.'] = 'u',
    ['>'] = 'v',
    ['/'] = 'w',
    ['?'] = 'x',
    -- reserved: y
    -- reserved: z
}

local DECODE_MAP = flip_table(ENCODE_MAP)

-- Z encodes a string.
-- Examples:
-- 'not=' -> 'notzm'
-- 'notzm' -> 'notzrm'
local function zencode(str)
    local zstr = ""
    
    for i = 1, #str do
        local chr = string.sub(str, i, i)
        local encoded_chr = ENCODE_MAP[chr]
        if encoded_chr == nil then
            zstr ..= chr
        else
            zstr ..= "z" .. encoded_chr
        end
    end

    return zstr
end

local function zdecode(zstr)
    local str = ""
    local decoding_state = false

    for i = 1, #zstr do
        local chr = string.sub(zstr, i, i)
        if decoding_state then
            local decoded_chr = DECODE_MAP[chr]
            if decoded_chr == nil then
                error("invalid encoding at position " .. i .. ": 'z" .. chr .. "'")
            end

            str ..= decoded_chr
            decoding_state = false
            
        elseif chr == "z" then
            decoding_state = true
        else
            str ..= chr
        end
    end

    return str
end

return {
    encode = zencode,
    decode = zdecode
}