<?php
// Enigma Machine — Rosetta Code Reference (PHP)
// Wehrmacht Enigma I: 3 rotors, Reflector B, plugboard.
// PeopleTec Inc. — Guinness World Record Attempt 2026

$FWD = ["EKMFLGDQVZNTOWYHXUSPAIBRCJ","AJDKSIRUXBLHWTMCQGZNPYFVOE","BDFHJLCPRTXVZNYEIWGAKMUSQO"];
$BWD = ["UWYGADFPVZBECKMTHXSLRINQOJ","AJPCZWRLFBDKOTYUQGENHXMIVS","TAGBPCSDQEUFVNZHYIXJWLRKOM"];
$NOTCH = [ord('Q'), ord('E'), ord('V')];
$REFLECTOR = "YRUHQSLDPXNGOKMIEBFZCWVJAT";

class Enigma {
    private $id, $off, $plug;
    function __construct($rotors, $key, $plugboard=[]) {
        global $FWD, $BWD, $NOTCH;
        $this->id = $rotors;
        $this->off = [ord($key[0])-65, ord($key[1])-65, ord($key[2])-65];
        $this->plug = range(0, 25);
        foreach ($plugboard as $p) {
            $a = ord($p[0])-65; $b = ord($p[1])-65;
            $this->plug[$a] = $b; $this->plug[$b] = $a;
        }
    }
    private function fwd($r, $i) {
        global $FWD;
        $c = ($i + $this->off[$r]) % 26;
        return (ord($FWD[$this->id[$r]][$c]) - 65 - $this->off[$r] + 26) % 26;
    }
    private function bwd($r, $i) {
        global $BWD;
        $c = ($i + $this->off[$r]) % 26;
        return (ord($BWD[$this->id[$r]][$c]) - 65 - $this->off[$r] + 26) % 26;
    }
    private function step() {
        global $NOTCH;
        if ($this->off[1]+65 == $NOTCH[$this->id[1]]) {
            $this->off[1] = ($this->off[1]+1)%26;
            $this->off[0] = ($this->off[0]+1)%26;
        } elseif ($this->off[2]+65 == $NOTCH[$this->id[2]]) {
            $this->off[1] = ($this->off[1]+1)%26;
        }
        $this->off[2] = ($this->off[2]+1)%26;
    }
    function pressKey($ch) {
        global $REFLECTOR;
        $this->step();
        $i = ord($ch) - 65;
        $i = $this->plug[$i];
        $i = $this->fwd(2,$i); $i = $this->fwd(1,$i); $i = $this->fwd(0,$i);
        $i = ord($REFLECTOR[$i]) - 65;
        $i = $this->bwd(0,$i); $i = $this->bwd(1,$i); $i = $this->bwd(2,$i);
        $i = $this->plug[$i];
        return chr($i + 65);
    }
    function encrypt($text) {
        $text = preg_replace('/[^A-Z]/', '', strtoupper($text));
        $out = '';
        for ($j = 0; $j < strlen($text); $j++) $out .= $this->pressKey($text[$j]);
        return $out;
    }
}

$tests = [
    [[0,1,2],"AAA",[],"AAAAA","BDZGO"],
    [[0,1,2],"AAA",[],"HELLOWORLD","ILBDAAMTAZ"],
    [[0,1,2],"AAA",[],"ATTACKATDAWN","BZHGNOCRRTCM"],
    [[0,1,2],"MCK",[],"HELLOWORLD","DLTBBQVPQV"],
    [[2,0,1],"AAA",[],"HELLOWORLD","KZHDFQYHXT"],
    [[0,1,2],"AAA",["AB","CD","EF"],"HELLOWORLD","IKACBBMTBF"],
];

$allOk = true;
foreach ($tests as $t => [$r,$k,$p,$pt,$exp]) {
    $e = new Enigma($r,$k,$p);
    $ct = $e->encrypt($pt);
    $ok = $ct === $exp;
    printf("Test %d: %-20s -> %-15s [%s]\n", $t+1, $pt, $ct, $ok?"PASS":"FAIL");
    if (!$ok) $allOk = false;
}
echo $allOk ? "\nALL 6 TESTS PASSED\n" : "\nSOME TESTS FAILED\n";
