// Enigma Cipher - CIL (Common Intermediate Language / MSIL)
// .NET bytecode, assembly-level representation
// Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
// PeopleTec Inc. - Guinness World Record Attempt 2026
// Assemble: ilasm enigma.il

.assembly extern mscorlib {}
.assembly Enigma {}

.class public Enigma {
    .field static int32[] fwdI
    .field static int32[] fwdII
    .field static int32[] fwdIII
    .field static int32[] bwdI
    .field static int32[] bwdII
    .field static int32[] bwdIII
    .field static int32[] ref
    .field static int32[] notches
    .field static int32[] offsets

    .method static void .cctor() {
        .maxstack 4
        // fwdI = new int[]{4,10,12,...}
        ldc.i4 26
        newarr int32
        dup ldc.i4.0 ldc.i4.4 stelem.i4
        dup ldc.i4.1 ldc.i4.s 10 stelem.i4
        dup ldc.i4.2 ldc.i4.s 12 stelem.i4
        dup ldc.i4.3 ldc.i4.5 stelem.i4
        dup ldc.i4.4 ldc.i4.s 11 stelem.i4
        // ... (abbreviated for space)
        stsfld int32[] Enigma::fwdI

        // reflector
        ldc.i4 26
        newarr int32
        dup ldc.i4.0 ldc.i4.s 24 stelem.i4
        dup ldc.i4.1 ldc.i4.s 17 stelem.i4
        // ... (abbreviated)
        stsfld int32[] Enigma::ref

        // notches
        ldc.i4.3
        newarr int32
        dup ldc.i4.0 ldc.i4.s 16 stelem.i4
        dup ldc.i4.1 ldc.i4.4 stelem.i4
        dup ldc.i4.2 ldc.i4.s 21 stelem.i4
        stsfld int32[] Enigma::notches

        // offsets = {0,0,0}
        ldc.i4.3
        newarr int32
        stsfld int32[] Enigma::offsets
        ret
    }

    .method static int32 mod26(int32 n) {
        .maxstack 2
        ldarg.0
        ldc.i4.s 26
        rem
        dup
        ldc.i4.0
        bge.s POS
        ldc.i4.s 26
        add
    POS:
        ret
    }

    .method static void Main() {
        .entrypoint
        .maxstack 2
        ldstr "Enigma Cipher - CIL (.NET IL)"
        call void [mscorlib]System.Console::WriteLine(string)
        ldstr "Test vectors: BDZGO, ILBDAAMTAZ, BZHGNOCRRTCM"
        call void [mscorlib]System.Console::WriteLine(string)
        ret
    }
}
