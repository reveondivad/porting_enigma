-- Enigma Machine — Rosetta Code Reference (Lua)
-- Wehrmacht Enigma I: 3 rotors, Reflector B, plugboard.
-- PeopleTec Inc. — Guinness World Record Attempt 2026

local FWD = {"EKMFLGDQVZNTOWYHXUSPAIBRCJ","AJDKSIRUXBLHWTMCQGZNPYFVOE","BDFHJLCPRTXVZNYEIWGAKMUSQO"}
local BWD = {"UWYGADFPVZBECKMTHXSLRINQOJ","AJPCZWRLFBDKOTYUQGENHXMIVS","TAGBPCSDQEUFVNZHYIXJWLRKOM"}
local NOTCH = {string.byte("Q"), string.byte("E"), string.byte("V")}
local REFLECTOR = "YRUHQSLDPXNGOKMIEBFZCWVJAT"

local function idx(c) return string.byte(c) - 65 end
local function chr(i) return string.char(i + 65) end

local Enigma = {}
Enigma.__index = Enigma

function Enigma.new(rotors, key, plugboard)
    local self = setmetatable({}, Enigma)
    -- Lua arrays are 1-based; rotor ids are 1-based here too
    self.id = {rotors[1]+1, rotors[2]+1, rotors[3]+1}
    self.off = {idx(key:sub(1,1)), idx(key:sub(2,2)), idx(key:sub(3,3))}
    self.plug = {}
    for i = 0, 25 do self.plug[i] = i end
    if plugboard then
        for _, p in ipairs(plugboard) do
            local a, b = idx(p:sub(1,1)), idx(p:sub(2,2))
            self.plug[a], self.plug[b] = b, a
        end
    end
    return self
end

function Enigma:fwd(r, i)
    local c = (i + self.off[r]) % 26
    local out = string.byte(FWD[self.id[r]], c+1) - 65
    return (out - self.off[r] + 26) % 26
end

function Enigma:bwd(r, i)
    local c = (i + self.off[r]) % 26
    local out = string.byte(BWD[self.id[r]], c+1) - 65
    return (out - self.off[r] + 26) % 26
end

function Enigma:step()
    if self.off[2] + 65 == NOTCH[self.id[2]] then
        self.off[2] = (self.off[2]+1)%26
        self.off[1] = (self.off[1]+1)%26
    elseif self.off[3] + 65 == NOTCH[self.id[3]] then
        self.off[2] = (self.off[2]+1)%26
    end
    self.off[3] = (self.off[3]+1)%26
end

function Enigma:pressKey(ch)
    self:step()
    local i = string.byte(ch) - 65
    i = self.plug[i]
    i = self:fwd(3,i); i = self:fwd(2,i); i = self:fwd(1,i)
    i = string.byte(REFLECTOR, i+1) - 65
    i = self:bwd(1,i); i = self:bwd(2,i); i = self:bwd(3,i)
    i = self.plug[i]
    return chr(i)
end

function Enigma:encrypt(text)
    local out = {}
    for c in text:upper():gmatch("[A-Z]") do
        out[#out+1] = self:pressKey(c)
    end
    return table.concat(out)
end

-- Test vectors
local tests = {
    {{0,1,2},"AAA",nil,"AAAAA","BDZGO"},
    {{0,1,2},"AAA",nil,"HELLOWORLD","ILBDAAMTAZ"},
    {{0,1,2},"AAA",nil,"ATTACKATDAWN","BZHGNOCRRTCM"},
    {{0,1,2},"MCK",nil,"HELLOWORLD","DLTBBQVPQV"},
    {{2,0,1},"AAA",nil,"HELLOWORLD","KZHDFQYHXT"},
    {{0,1,2},"AAA",{"AB","CD","EF"},"HELLOWORLD","IKACBBMTBF"},
}

local allOk = true
for t, tv in ipairs(tests) do
    local e = Enigma.new(tv[1],tv[2],tv[3])
    local ct = e:encrypt(tv[4])
    local ok = ct == tv[5]
    print(string.format("Test %d: %-20s -> %-15s [%s]", t, tv[4], ct, ok and "PASS" or "FAIL"))
    if not ok then allOk = false end
end
print(allOk and "\nALL 6 TESTS PASSED" or "\nSOME TESTS FAILED")
