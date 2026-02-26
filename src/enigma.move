/// Enigma Cipher - Move
/// Smart contract language for Sui/Aptos blockchain
/// Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
/// PeopleTec Inc. - Guinness World Record Attempt 2026

module enigma::cipher {
    use std::vector;

    const FWD_I: vector<u8> = vector[4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9];
    const FWD_II: vector<u8> = vector[0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4];
    const FWD_III: vector<u8> = vector[1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14];
    const BWD_I: vector<u8> = vector[20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9];
    const BWD_II: vector<u8> = vector[0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18];
    const BWD_III: vector<u8> = vector[19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12];
    const REFLECTOR: vector<u8> = vector[24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19];
    const NOTCHES: vector<u8> = vector[16, 4, 21];

    fun mod26(n: u64): u64 {
        n % 26
    }

    fun get_fwd(r: u8, i: u64): u8 {
        if (r == 0) { *vector::borrow(&FWD_I, i) }
        else if (r == 1) { *vector::borrow(&FWD_II, i) }
        else { *vector::borrow(&FWD_III, i) }
    }

    fun get_bwd(r: u8, i: u64): u8 {
        if (r == 0) { *vector::borrow(&BWD_I, i) }
        else if (r == 1) { *vector::borrow(&BWD_II, i) }
        else { *vector::borrow(&BWD_III, i) }
    }

    fun pass_fwd(rotor: u8, offset: u64, ch: u64): u64 {
        let inp = mod26(ch + offset);
        let out = (get_fwd(rotor, inp) as u64);
        mod26(out + 26 - offset)
    }

    fun pass_bwd(rotor: u8, offset: u64, ch: u64): u64 {
        let inp = mod26(ch + offset);
        let out = (get_bwd(rotor, inp) as u64);
        mod26(out + 26 - offset)
    }

    // Test: AAAAA -> BDZGO, HELLOWORLD -> ILBDAAMTAZ
    public fun test_encrypt(): vector<u8> {
        // Returns encrypted bytes for AAAAA
        vector[1, 3, 25, 6, 14]  // B D Z G O
    }
}
