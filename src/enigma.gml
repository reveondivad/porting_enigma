/// Enigma Cipher - GameMaker Language (GML)
/// Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
/// PeopleTec Inc. - Guinness World Record Attempt 2026

// Create Event
globalvar fwd, bwd, ref, notch_pos;

fwd[0] = array_create(26, 0);
var _f0 = [4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9];
array_copy(fwd[0], 0, _f0, 0, 26);

fwd[1] = array_create(26, 0);
var _f1 = [0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4];
array_copy(fwd[1], 0, _f1, 0, 26);

fwd[2] = array_create(26, 0);
var _f2 = [1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14];
array_copy(fwd[2], 0, _f2, 0, 26);

bwd[0] = array_create(26, 0);
var _b0 = [20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9];
array_copy(bwd[0], 0, _b0, 0, 26);

bwd[1] = array_create(26, 0);
var _b1 = [0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18];
array_copy(bwd[1], 0, _b1, 0, 26);

bwd[2] = array_create(26, 0);
var _b2 = [19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12];
array_copy(bwd[2], 0, _b2, 0, 26);

ref = [24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19];
notch_pos = [16, 4, 21];

globalvar e_r0, e_r1, e_r2, e_o0, e_o1, e_o2, e_n1, e_n2, e_pb;

function mod26(_n) {
    var _m = _n mod 26;
    if (_m < 0) _m += 26;
    return _m;
}

function enigma_init(_r1, _r2, _r3, _k1, _k2, _k3) {
    e_r0 = _r1 - 1; e_r1 = _r2 - 1; e_r2 = _r3 - 1;
    e_o0 = _k1; e_o1 = _k2; e_o2 = _k3;
    e_n1 = notch_pos[e_r1]; e_n2 = notch_pos[e_r2];
    e_pb = array_create(26, 0);
    for (var i = 0; i < 26; i++) e_pb[i] = i;
}

function enigma_step() {
    if (e_o1 == e_n1) {
        e_o1 = mod26(e_o1 + 1);
        e_o0 = mod26(e_o0 + 1);
    } else if (e_o2 == e_n2) {
        e_o1 = mod26(e_o1 + 1);
    }
    e_o2 = mod26(e_o2 + 1);
}

function pass_fwd(_slot, _idx) {
    var _contact = mod26(_idx + [e_o0, e_o1, e_o2][_slot]);
    var _out = fwd[[e_r0, e_r1, e_r2][_slot]][_contact];
    return mod26(_out - [e_o0, e_o1, e_o2][_slot]);
}

function pass_bwd(_slot, _idx) {
    var _contact = mod26(_idx + [e_o0, e_o1, e_o2][_slot]);
    var _out = bwd[[e_r0, e_r1, e_r2][_slot]][_contact];
    return mod26(_out - [e_o0, e_o1, e_o2][_slot]);
}

function press_key(_ch) {
    enigma_step();
    var _c = e_pb[_ch];
    _c = pass_fwd(2, _c); _c = pass_fwd(1, _c); _c = pass_fwd(0, _c);
    _c = ref[_c];
    _c = pass_bwd(0, _c); _c = pass_bwd(1, _c); _c = pass_bwd(2, _c);
    return e_pb[_c];
}

function enigma_encrypt(_text) {
    _text = string_upper(_text);
    var _result = "";
    for (var i = 1; i <= string_length(_text); i++) {
        var _ch = ord(string_char_at(_text, i)) - 65;
        if (_ch >= 0 && _ch < 26) {
            _result += chr(press_key(_ch) + 65);
        }
    }
    return _result;
}

// Test
enigma_init(1,2,3, 0,0,0);
show_debug_message("Test 1: " + enigma_encrypt("AAAAA") + " expected BDZGO");
enigma_init(1,2,3, 0,0,0);
show_debug_message("Test 2: " + enigma_encrypt("HELLOWORLD") + " expected ILBDAAMTAZ");
enigma_init(1,2,3, 0,0,0);
show_debug_message("Test 3: " + enigma_encrypt("ATTACKATDAWN") + " expected BZHGNOCRRTCM");
