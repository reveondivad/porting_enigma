* Enigma cipher in Stata
program define enigma
    args text
    
    * Rotor I forward
    matrix rf1 = (4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9)
    matrix rf2 = (0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4)
    matrix rf3 = (1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14)
    matrix rb1 = (20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9)
    matrix rb2 = (0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18)
    matrix rb3 = (19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12)
    matrix ref = (24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19)
    
    local notch0 = 16
    local notch1 = 4
    local notch2 = 21
    local p0 = 0
    local p1 = 0
    local p2 = 0
    local result = ""
    
    local text = strupper("`text'")
    local len = strlen("`text'")
    
    forvalues i = 1/`len' {
        local ch = substr("`text'", `i', 1)
        local c = char(`ch') - 65
        if `c' >= 0 & `c' < 26 {
            * Step rotors
            local mid = (`p1' == `notch1')
            if `p2' == `notch2' {
                local p2 = mod(`p2' + 1, 26)
            }
            if `mid' | `p2' == `notch2' {
                local p1 = mod(`p1' + 1, 26)
            }
            local p2 = mod(`p2' + 1, 26)
            * Forward through rotor 3
            local idx = mod(`c' + `p2', 26) + 1
            local c = mod(rf3[1, `idx'] - `p2' + 26, 26)
            local result = "`result'" + char(`c' + 65)
        }
    }
    display "`result'"
end

enigma "HELLOWORLD"
