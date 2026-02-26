// Enigma cipher in Q# (quantum-classical hybrid)
namespace Enigma {
    open Microsoft.Quantum.Convert;
    open Microsoft.Quantum.Arrays;
    open Microsoft.Quantum.Math;

    function Mod26(n : Int) : Int {
        let m = n % 26;
        return m < 0 ? m + 26 | m;
    }

    function RotorFwd() : Int[][] {
        return [
            [4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9],
            [0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4],
            [1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14]
        ];
    }

    function RotorBwd() : Int[][] {
        return [
            [20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9],
            [0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18],
            [19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12]
        ];
    }

    function Reflector() : Int[] {
        return [24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19];
    }

    function Notches() : Int[] { return [16, 4, 21]; }

    function RotorPass(wiring : Int[], c : Int, pos : Int) : Int {
        return Mod26(wiring[Mod26(c + pos)] - pos);
    }

    function EncryptChar(c : Int, pos : Int[]) : Int {
        let fwd = RotorFwd();
        let bwd = RotorBwd();
        let ref = Reflector();
        mutable x = c;
        for i in 2..-1..0 { set x = RotorPass(fwd[i], x, pos[i]); }
        set x = ref[x];
        for i in 0..2 { set x = RotorPass(bwd[i], x, pos[i]); }
        return x;
    }

    @EntryPoint()
    operation Main() : Unit {
        Message("Enigma Q# implementation ready");
    }
}
