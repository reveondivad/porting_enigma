# Enigma Machine - PowerShell Implementation
# Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping)
# PeopleTec Inc. - Guinness World Record Attempt 2026

$FWD = @("EKMFLGDQVZNTOWYHXUSPAIBRCJ","AJDKSIRUXBLHWTMCQGZNPYFVOE","BDFHJLCPRTXVZNYEIWGAKMUSQO")
$BWD = @("UWYGADFPVZBECKMTHXSLRINQOJ","AJPCZWRLFBDKOTYUQGENHXMIVS","TAGBPCSDQEUFVNZHYIXJWLRKOM")
$NOTCH = @(16, 4, 21)
$REFL = "YRUHQSLDPXNGOKMIEBFZCWVJAT"

function Mod26($a) { (($a % 26) + 26) % 26 }
function C2I($c) { [int][char]$c - [int][char]'A' }
function I2C($i) { [char]($i + [int][char]'A') }

function New-Enigma($rotors, $key, $plugboard = @()) {
    $state = @{
        left_fwd = $FWD[$rotors[0]]; left_bwd = $BWD[$rotors[0]]; left_notch = $NOTCH[$rotors[0]]
        left_off = C2I $key[0]
        mid_fwd = $FWD[$rotors[1]]; mid_bwd = $BWD[$rotors[1]]; mid_notch = $NOTCH[$rotors[1]]
        mid_off = C2I $key[1]
        right_fwd = $FWD[$rotors[2]]; right_bwd = $BWD[$rotors[2]]; right_notch = $NOTCH[$rotors[2]]
        right_off = C2I $key[2]
        plug = @(0..25)
    }
    foreach ($pair in $plugboard) {
        $a = C2I $pair[0]; $b = C2I $pair[1]
        $state.plug[$a] = $b; $state.plug[$b] = $a
    }
    return $state
}

function FwdPass($wiring, $offset, $idx) {
    $contact = Mod26 ($idx + $offset)
    $out = C2I $wiring[$contact]
    Mod26 ($out - $offset)
}

function BwdPass($wiring, $offset, $idx) {
    $contact = Mod26 ($idx + $offset)
    $out = C2I $wiring[$contact]
    Mod26 ($out - $offset)
}

function Step-Rotors($s) {
    if ($s.mid_off -eq $s.mid_notch) {
        $s.mid_off = ($s.mid_off + 1) % 26
        $s.left_off = ($s.left_off + 1) % 26
    } elseif ($s.right_off -eq $s.right_notch) {
        $s.mid_off = ($s.mid_off + 1) % 26
    }
    $s.right_off = ($s.right_off + 1) % 26
}

function Press-Key($s, $c) {
    Step-Rotors $s
    $idx = C2I $c
    $idx = $s.plug[$idx]
    $idx = FwdPass $s.right_fwd $s.right_off $idx
    $idx = FwdPass $s.mid_fwd $s.mid_off $idx
    $idx = FwdPass $s.left_fwd $s.left_off $idx
    $idx = C2I $REFL[$idx]
    $idx = BwdPass $s.left_bwd $s.left_off $idx
    $idx = BwdPass $s.mid_bwd $s.mid_off $idx
    $idx = BwdPass $s.right_bwd $s.right_off $idx
    $idx = $s.plug[$idx]
    I2C $idx
}

function Encrypt-Text($s, $text) {
    $upper = $text.ToUpper()
    $result = ""
    foreach ($c in $upper.ToCharArray()) {
        if ($c -ge 'A' -and $c -le 'Z') {
            $result += Press-Key $s $c
        }
    }
    return $result
}

Write-Host "Enigma Machine - PowerShell Implementation"
Write-Host "==========================================="

$tests = @(
    @(@(0,1,2), "AAA", @(),            "AAAAA",        "BDZGO"),
    @(@(0,1,2), "AAA", @(),            "HELLOWORLD",   "ILBDAAMTAZ"),
    @(@(0,1,2), "AAA", @(),            "ATTACKATDAWN", "BZHGNOCRRTCM"),
    @(@(0,1,2), "MCK", @(),            "HELLOWORLD",   "DLTBBQVPQV"),
    @(@(2,0,1), "AAA", @(),            "HELLOWORLD",   "KZHDFQYHXT"),
    @(@(0,1,2), "AAA", @("AB","CD","EF"), "HELLOWORLD", "IKACBBMTBF")
)

$allPass = $true
for ($i = 0; $i -lt $tests.Count; $i++) {
    $t = $tests[$i]
    $e = New-Enigma $t[0] $t[1] $t[2]
    $cipher = Encrypt-Text $e $t[3]
    $ok = $cipher -eq $t[4]
    $status = if ($ok) { "PASS" } else { "FAIL" }
    Write-Host ("  Test {0}: {1,-20} -> {2,-15} [{3}]" -f ($i+1), $t[3], $cipher, $status)
    if (-not $ok) { Write-Host ("          Expected {0}, got {1}" -f $t[4], $cipher); $allPass = $false }
}
Write-Host $(if ($allPass) { "`n  ALL 6 TESTS PASSED" } else { "`n  SOME TESTS FAILED" })
