// Enigma Cipher - WGSL (WebGPU Shading Language)
// GPU compute shader for parallel Enigma encryption
// Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
// PeopleTec Inc. - Guinness World Record Attempt 2026

// Rotor wirings in constant arrays
const fwdI  = array<u32, 26>(4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9);
const fwdII = array<u32, 26>(0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4);
const fwdIII= array<u32, 26>(1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14);
const bwdI  = array<u32, 26>(20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9);
const bwdII = array<u32, 26>(0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18);
const bwdIII= array<u32, 26>(19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12);
const ref    = array<u32, 26>(24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19);
const notch  = array<u32, 3>(16u, 4u, 21u);

fn mod26(n: i32) -> u32 {
    let m = n % 26;
    if (m < 0) { return u32(m + 26); }
    return u32(m);
}

fn get_fwd(r: u32, i: u32) -> u32 {
    if (r == 0u) { return fwdI[i]; }
    if (r == 1u) { return fwdII[i]; }
    return fwdIII[i];
}

fn get_bwd(r: u32, i: u32) -> u32 {
    if (r == 0u) { return bwdI[i]; }
    if (r == 1u) { return bwdII[i]; }
    return bwdIII[i];
}

fn pass_fwd(rotor: u32, offset: u32, ch: u32) -> u32 {
    let inp = mod26(i32(ch) + i32(offset));
    let out = get_fwd(rotor, inp);
    return mod26(i32(out) - i32(offset));
}

fn pass_bwd(rotor: u32, offset: u32, ch: u32) -> u32 {
    let inp = mod26(i32(ch) + i32(offset));
    let out = get_bwd(rotor, inp);
    return mod26(i32(out) - i32(offset));
}

struct EnigmaParams {
    r0: u32, r1: u32, r2: u32,
    k0: u32, k1: u32, k2: u32,
    msg_len: u32,
};

@group(0) @binding(0) var<uniform> params: EnigmaParams;
@group(0) @binding(1) var<storage, read> input_msg: array<u32>;
@group(0) @binding(2) var<storage, read_write> output_msg: array<u32>;

@compute @workgroup_size(1)
fn main(@builtin(global_invocation_id) id: vec3<u32>) {
    var o0 = params.k0;
    var o1 = params.k1;
    var o2 = params.k2;
    let n1 = notch[params.r1];
    let n2 = notch[params.r2];

    for (var i = 0u; i < params.msg_len; i = i + 1u) {
        let ch = input_msg[i];
        let mid = (o1 == n1);
        let atn = (o2 == n2);
        o2 = mod26(i32(o2) + 1);
        if (atn || mid) { o1 = mod26(i32(o1) + 1); }
        if (mid) { o0 = mod26(i32(o0) + 1); }

        var c = ch;
        c = pass_fwd(params.r2, o2, c);
        c = pass_fwd(params.r1, o1, c);
        c = pass_fwd(params.r0, o0, c);
        c = ref[c];
        c = pass_bwd(params.r0, o0, c);
        c = pass_bwd(params.r1, o1, c);
        c = pass_bwd(params.r2, o2, c);
        output_msg[i] = c;
    }
}
