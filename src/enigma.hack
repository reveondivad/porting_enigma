<?hh // strict
// Enigma Cipher - Hack
// Facebook's statically-typed PHP evolution

namespace Enigma;

const vec<int> FWD_I = vec[4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9];
const vec<int> FWD_II = vec[0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4];
const vec<int> FWD_III = vec[1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14];

const vec<int> BWD_I = vec[20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9];
const vec<int> BWD_II = vec[0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18];
const vec<int> BWD_III = vec[19,0,6,1,15,2,18,3,16,4,20,9,21,13,25,7,24,8,23,5,22,11,17,12,14,10];

const vec<int> NOTCHES = vec[16, 4, 21];
const vec<int> REFLECTOR = vec[24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19];

function mod26(int $x): int {
    $m = $x % 26;
    return $m < 0 ? $m + 26 : $m;
}

function get_fwd(int $r, int $i): int {
    return match ($r) {
        0 => FWD_I[$i],
        1 => FWD_II[$i],
        default => FWD_III[$i],
    };
}

function get_bwd(int $r, int $i): int {
    return match ($r) {
        0 => BWD_I[$i],
        1 => BWD_II[$i],
        default => BWD_III[$i],
    };
}

function pass_fwd(int $rotor, int $offset, int $ch): int {
    $inp = mod26($ch + $offset);
    $out = get_fwd($rotor, $inp);
    return mod26($out - $offset);
}

function pass_bwd(int $rotor, int $offset, int $ch): int {
    $inp = mod26($ch + $offset);
    $out = get_bwd($rotor, $inp);
    return mod26($out - $offset);
}

function make_plugboard(vec<(int, int)> $pairs): vec<int> {
    $pb = vec[];
    for ($i = 0; $i < 26; $i++) { $pb[] = $i; }
    foreach ($pairs as list($a, $b)) {
        $pb[$a] = $b;
        $pb[$b] = $a;
    }
    return $pb;
}

function encrypt(
    int $r0, int $r1, int $r2,
    int $k0, int $k1, int $k2,
    vec<(int, int)> $pairs,
    string $msg,
): string {
    $pb = make_plugboard($pairs);
    $o0 = $k0; $o1 = $k1; $o2 = $k2;
    $result = '';
    for ($i = 0; $i < \strlen($msg); $i++) {
        $ch = \ord($msg[$i]) - 65;
        $mid = $o1 === NOTCHES[$r1];
        $atn = $o2 === NOTCHES[$r2];
        $o2 = mod26($o2 + 1);
        if ($atn || $mid) { $o1 = mod26($o1 + 1); }
        if ($mid) { $o0 = mod26($o0 + 1); }
        $c = $pb[$ch];
        $c = pass_fwd($r2, $o2, $c);
        $c = pass_fwd($r1, $o1, $c);
        $c = pass_fwd($r0, $o0, $c);
        $c = REFLECTOR[$c];
        $c = pass_bwd($r0, $o0, $c);
        $c = pass_bwd($r1, $o1, $c);
        $c = pass_bwd($r2, $o2, $c);
        $c = $pb[$c];
        $result .= \chr($c + 65);
    }
    return $result;
}

function run_test(string $label, string $expected, string $actual): void {
    $status = $expected === $actual ? "PASS" : "FAIL";
    echo "$status $label: $actual (expected $expected)\n";
}

<<__EntryPoint>>
function main(): void {
    echo "Enigma Cipher - Hack\n";
    $no = vec[];
    run_test("Test 1", "BDZGO", encrypt(0,1,2, 0,0,0, $no, "AAAAA"));
    run_test("Test 2", "ILBDAAMTAZ", encrypt(0,1,2, 0,0,0, $no, "HELLOWORLD"));
    run_test("Test 3", "BZHGNOCRRTCM", encrypt(0,1,2, 0,0,0, $no, "ATTACKATDAWN"));
    run_test("Test 4", "DLTBBQVPQV", encrypt(0,1,2, 12,2,10, $no, "HELLOWORLD"));
    run_test("Test 5", "KZHDFQYHXT", encrypt(2,0,1, 0,0,0, $no, "HELLOWORLD"));
    run_test("Test 6", "IKACBBMTBF", encrypt(0,1,2, 0,0,0, vec[tuple(0,1),tuple(2,3),tuple(4,5)], "HELLOWORLD"));
}
