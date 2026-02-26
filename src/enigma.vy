# Enigma Cipher - Vyper
# Pythonic smart contract language for Ethereum

# @version ^0.3.0

FWD_I: constant(int128[26]) = [4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9]
FWD_II: constant(int128[26]) = [0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4]
FWD_III: constant(int128[26]) = [1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14]

BWD_I: constant(int128[26]) = [20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9]
BWD_II: constant(int128[26]) = [0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18]
BWD_III: constant(int128[26]) = [19,0,6,1,15,2,18,3,16,4,20,9,21,13,25,7,24,8,23,5,22,11,17,12,14,10]

NOTCHES: constant(int128[3]) = [16, 4, 21]
REFLECTOR: constant(int128[26]) = [24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19]

@internal
@pure
def _mod26(x: int128) -> int128:
    m: int128 = x % 26
    if m < 0:
        m += 26
    return m

@internal
@pure
def _get_fwd(r: int128, i: int128) -> int128:
    if r == 0:
        return FWD_I[i]
    elif r == 1:
        return FWD_II[i]
    else:
        return FWD_III[i]

@internal
@pure
def _get_bwd(r: int128, i: int128) -> int128:
    if r == 0:
        return BWD_I[i]
    elif r == 1:
        return BWD_II[i]
    else:
        return BWD_III[i]

@internal
@pure
def _pass_fwd(rotor: int128, offset: int128, ch: int128) -> int128:
    inp: int128 = self._mod26(ch + offset)
    out: int128 = self._get_fwd(rotor, inp)
    return self._mod26(out - offset)

@internal
@pure
def _pass_bwd(rotor: int128, offset: int128, ch: int128) -> int128:
    inp: int128 = self._mod26(ch + offset)
    out: int128 = self._get_bwd(rotor, inp)
    return self._mod26(out - offset)

@external
@view
def encrypt(r0: int128, r1: int128, r2: int128,
            k0: int128, k1: int128, k2: int128,
            msg: int128[64], msg_len: int128) -> int128[64]:
    """Encrypt a message (as array of 0-25 integers)"""
    o0: int128 = k0
    o1: int128 = k1
    o2: int128 = k2
    result: int128[64] = empty(int128[64])

    for i in range(64):
        if i >= msg_len:
            break
        ch: int128 = msg[i]
        mid: bool = o1 == NOTCHES[r1]
        atn: bool = o2 == NOTCHES[r2]
        o2 = self._mod26(o2 + 1)
        if atn or mid:
            o1 = self._mod26(o1 + 1)
        if mid:
            o0 = self._mod26(o0 + 1)
        c: int128 = ch  # no plugboard in this simplified version
        c = self._pass_fwd(r2, o2, c)
        c = self._pass_fwd(r1, o1, c)
        c = self._pass_fwd(r0, o0, c)
        c = REFLECTOR[c]
        c = self._pass_bwd(r0, o0, c)
        c = self._pass_bwd(r1, o1, c)
        c = self._pass_bwd(r2, o2, c)
        result[i] = c

    return result

# Test vectors (called via external tools):
# encrypt(0,1,2, 0,0,0, [0,0,0,0,0,...], 5) -> [1,3,25,6,14,...] (BDZGO)
# encrypt(0,1,2, 0,0,0, [7,4,11,11,14,22,14,17,11,3,...], 10) -> ILBDAAMTAZ
