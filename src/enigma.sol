// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

/// @title Enigma Cipher - Solidity
/// @notice Wehrmacht Enigma I implementation as an Ethereum smart contract

contract EnigmaCipher {

    uint8[26] private reflectorB = [24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19];
    uint8[3] private notches = [16, 4, 21];

    uint8[26][3] private fwd;
    uint8[26][3] private bwd;

    constructor() {
        fwd[0] = [4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9];
        fwd[1] = [0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4];
        fwd[2] = [1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14];

        bwd[0] = [20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9];
        bwd[1] = [0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18];
        bwd[2] = [19,0,6,1,15,2,18,3,16,4,20,9,21,13,25,7,24,8,23,5,22,11,17,12,14,10];
    }

    function mod26(int256 x) private pure returns (uint8) {
        int256 m = x % 26;
        if (m < 0) m += 26;
        return uint8(uint256(m));
    }

    function passFwd(uint8 rotor, uint8 offset, uint8 ch) private view returns (uint8) {
        uint8 inp = mod26(int256(uint256(ch)) + int256(uint256(offset)));
        uint8 out = fwd[rotor][inp];
        return mod26(int256(uint256(out)) - int256(uint256(offset)));
    }

    function passBwd(uint8 rotor, uint8 offset, uint8 ch) private view returns (uint8) {
        uint8 inp = mod26(int256(uint256(ch)) + int256(uint256(offset)));
        uint8 out = bwd[rotor][inp];
        return mod26(int256(uint256(out)) - int256(uint256(offset)));
    }

    function encrypt(
        uint8 r0, uint8 r1, uint8 r2,
        uint8 k0, uint8 k1, uint8 k2,
        uint8[] memory pbPairsA, uint8[] memory pbPairsB,
        bytes memory msg
    ) public view returns (bytes memory) {
        // Build plugboard
        uint8[26] memory pb;
        for (uint8 i = 0; i < 26; i++) { pb[i] = i; }
        for (uint i = 0; i < pbPairsA.length; i++) {
            pb[pbPairsA[i]] = pbPairsB[i];
            pb[pbPairsB[i]] = pbPairsA[i];
        }

        uint8 o0 = k0;
        uint8 o1 = k1;
        uint8 o2 = k2;
        bytes memory result = new bytes(msg.length);

        for (uint i = 0; i < msg.length; i++) {
            uint8 ch = uint8(msg[i]) - 65;
            bool mid = (o1 == notches[r1]);
            bool atn = (o2 == notches[r2]);
            o2 = mod26(int256(uint256(o2)) + 1);
            if (atn || mid) { o1 = mod26(int256(uint256(o1)) + 1); }
            if (mid) { o0 = mod26(int256(uint256(o0)) + 1); }

            uint8 c = pb[ch];
            c = passFwd(r2, o2, c);
            c = passFwd(r1, o1, c);
            c = passFwd(r0, o0, c);
            c = reflectorB[c];
            c = passBwd(r0, o0, c);
            c = passBwd(r1, o1, c);
            c = passBwd(r2, o2, c);
            c = pb[c];
            result[i] = bytes1(c + 65);
        }
        return result;
    }

    // Test function - returns all test results
    function runTests() public view returns (
        bytes memory t1, bytes memory t2, bytes memory t3,
        bytes memory t4, bytes memory t5, bytes memory t6
    ) {
        uint8[] memory noA = new uint8[](0);
        uint8[] memory noB = new uint8[](0);

        t1 = encrypt(0,1,2, 0,0,0, noA, noB, "AAAAA");         // BDZGO
        t2 = encrypt(0,1,2, 0,0,0, noA, noB, "HELLOWORLD");    // ILBDAAMTAZ
        t3 = encrypt(0,1,2, 0,0,0, noA, noB, "ATTACKATDAWN");  // BZHGNOCRRTCM
        t4 = encrypt(0,1,2, 12,2,10, noA, noB, "HELLOWORLD");  // DLTBBQVPQV
        t5 = encrypt(2,0,1, 0,0,0, noA, noB, "HELLOWORLD");    // KZHDFQYHXT

        uint8[] memory pA = new uint8[](3);
        uint8[] memory pB = new uint8[](3);
        pA[0]=0; pB[0]=1; pA[1]=2; pB[1]=3; pA[2]=4; pB[2]=5;
        t6 = encrypt(0,1,2, 0,0,0, pA, pB, "HELLOWORLD");      // IKACBBMTBF
    }
}
