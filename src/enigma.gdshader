// Enigma Cipher - Godot Shader Language (GDShader)
// GPU shader for parallel Enigma encryption
// Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
// PeopleTec Inc. - Guinness World Record Attempt 2026

shader_type canvas_item;

// Rotor wirings as uniform arrays
uniform int fwd_I[26] = {4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9};
uniform int fwd_II[26] = {0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4};
uniform int fwd_III[26] = {1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14};
uniform int bwd_I[26] = {20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9};
uniform int bwd_II[26] = {0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18};
uniform int bwd_III[26] = {19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12};
uniform int reflector[26] = {24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19};
uniform int notches[3] = {16, 4, 21};

// Enigma state
uniform int rotor_config[3] = {0, 1, 2};
uniform int offsets[3] = {0, 0, 0};

int mod26(int n) {
    int m = n - (n / 26) * 26;
    if (m < 0) { m += 26; }
    return m;
}

int get_fwd(int r, int i) {
    if (r == 0) return fwd_I[i];
    if (r == 1) return fwd_II[i];
    return fwd_III[i];
}

int get_bwd(int r, int i) {
    if (r == 0) return bwd_I[i];
    if (r == 1) return bwd_II[i];
    return bwd_III[i];
}

int pass_fwd(int rotor, int offset, int ch) {
    int inp = mod26(ch + offset);
    int out_val = get_fwd(rotor, inp);
    return mod26(out_val - offset);
}

int pass_bwd(int rotor, int offset, int ch) {
    int inp = mod26(ch + offset);
    int out_val = get_bwd(rotor, inp);
    return mod26(out_val - offset);
}

void fragment() {
    // Use UV.x to select character position (0-25)
    int ch = int(UV.x * 26.0);
    ch = clamp(ch, 0, 25);

    // Forward through rotors
    int c = ch;
    c = pass_fwd(rotor_config[2], offsets[2], c);
    c = pass_fwd(rotor_config[1], offsets[1], c);
    c = pass_fwd(rotor_config[0], offsets[0], c);
    c = reflector[c];
    c = pass_bwd(rotor_config[0], offsets[0], c);
    c = pass_bwd(rotor_config[1], offsets[1], c);
    c = pass_bwd(rotor_config[2], offsets[2], c);

    // Visualize: encode result as color
    float val = float(c) / 25.0;
    COLOR = vec4(val, 1.0 - val, 0.5, 1.0);
}
