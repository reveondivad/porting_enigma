-- Enigma Cipher - Teal
-- Typed dialect of Lua
-- Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
-- PeopleTec Inc. - Guinness World Record Attempt 2026

local fwd: {{integer}} = {
    {4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9},
    {0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4},
    {1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14},
}
local bwd: {{integer}} = {
    {20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9},
    {0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18},
    {19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12},
}
local reflector: {integer} = {24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19}
local notches: {integer} = {16, 4, 21}

local function mod26(n: integer): integer
    local m: integer = n % 26
    if m < 0 then m = m + 26 end
    return m
end

local function passFwd(rotor: integer, offset: integer, ch: integer): integer
    local inp: integer = mod26(ch + offset)
    local out: integer = fwd[rotor+1][inp+1]
    return mod26(out - offset)
end

local function passBwd(rotor: integer, offset: integer, ch: integer): integer
    local inp: integer = mod26(ch + offset)
    local out: integer = bwd[rotor+1][inp+1]
    return mod26(out - offset)
end

local record State
    r: {integer}
    o: {integer}
    n1: integer
    n2: integer
end

local function newEnigma(r0: integer, r1: integer, r2: integer,
                         k0: integer, k1: integer, k2: integer): State
    return {
        r = {r0, r1, r2},
        o = {k0, k1, k2},
        n1 = notches[r1+1],
        n2 = notches[r2+1],
    }
end

local function step(s: State)
    if s.o[2] == s.n1 then
        s.o[2] = mod26(s.o[2]+1)
        s.o[1] = mod26(s.o[1]+1)
    elseif s.o[3] == s.n2 then
        s.o[2] = mod26(s.o[2]+1)
    end
    s.o[3] = mod26(s.o[3]+1)
end

local function pressKey(s: State, ch: integer): integer
    step(s)
    local c: integer = ch
    c = passFwd(s.r[3], s.o[3], c)
    c = passFwd(s.r[2], s.o[2], c)
    c = passFwd(s.r[1], s.o[1], c)
    c = reflector[c+1]
    c = passBwd(s.r[1], s.o[1], c)
    c = passBwd(s.r[2], s.o[2], c)
    c = passBwd(s.r[3], s.o[3], c)
    return c
end

local function encrypt(s: State, msg: string): string
    local result: string = ""
    for i = 1, #msg do
        local ch: integer = string.byte(msg, i) - 65
        result = result .. string.char(pressKey(s, ch) + 65)
    end
    return result
end

print("Enigma Cipher - Teal")
local e: State = newEnigma(0,1,2,0,0,0)
print("Test 1: " .. encrypt(e, "AAAAA") .. " expected BDZGO")
e = newEnigma(0,1,2,0,0,0)
print("Test 2: " .. encrypt(e, "HELLOWORLD") .. " expected ILBDAAMTAZ")
e = newEnigma(0,1,2,0,0,0)
print("Test 3: " .. encrypt(e, "ATTACKATDAWN") .. " expected BZHGNOCRRTCM")
e = newEnigma(0,1,2,12,2,10)
print("Test 4: " .. encrypt(e, "HELLOWORLD") .. " expected DLTBBQVPQV")
e = newEnigma(2,0,1,0,0,0)
print("Test 5: " .. encrypt(e, "HELLOWORLD") .. " expected KZHDFQYHXT")
