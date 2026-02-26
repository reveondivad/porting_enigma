#!/usr/bin/env raku
# Enigma Machine - Raku (Perl 6) Implementation
# Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping)
# PeopleTec Inc. - Guinness World Record Attempt 2026

my @FWD = <EKMFLGDQVZNTOWYHXUSPAIBRCJ AJDKSIRUXBLHWTMCQGZNPYFVOE BDFHJLCPRTXVZNYEIWGAKMUSQO>;
my @BWD = <UWYGADFPVZBECKMTHXSLRINQOJ AJPCZWRLFBDKOTYUQGENHXMIVS TAGBPCSDQEUFVNZHYIXJWLRKOM>;
my $REF = "YRUHQSLDPXNGOKMIEBFZCWVJAT";
my @NOTCH = (16, 4, 21);

class Enigma {
    has @.rotors;
    has @.offsets is rw;
    has @.notches;
    has @.plugboard is rw;

    method new(Int :$r1, Int :$r2, Int :$r3, Str :$k1, Str :$k2, Str :$k3, Str :$plug = "") {
        my @r = ($r1 - 1, $r2 - 1, $r3 - 1);
        my @o = ($k1.ord - 65, $k2.ord - 65, $k3.ord - 65);
        my @n = (@NOTCH[@r[0]], @NOTCH[@r[1]], @NOTCH[@r[2]]);
        my @p = (0..25).list;
        if $plug {
            for $plug.split("-") -> $pair {
                my $a = $pair.substr(0,1).ord - 65;
                my $b = $pair.substr(1,1).ord - 65;
                @p[$a] = $b;
                @p[$b] = $a;
            }
        }
        self.bless(rotors => @r, offsets => @o, notches => @n, plugboard => @p);
    }

    sub mod26(Int $n --> Int) { (($n % 26) + 26) % 26 }

    method step() {
        if @!offsets[1] == @!notches[1] {
            @!offsets[1] = mod26(@!offsets[1] + 1);
            @!offsets[0] = mod26(@!offsets[0] + 1);
        } elsif @!offsets[2] == @!notches[2] {
            @!offsets[1] = mod26(@!offsets[1] + 1);
        }
        @!offsets[2] = mod26(@!offsets[2] + 1);
    }

    method fwd-pass(Int $rotor, Int $idx --> Int) {
        my $contact = mod26($idx + @!offsets[$rotor]);
        my $out = @FWD[@!rotors[$rotor]].substr($contact, 1).ord - 65;
        mod26($out - @!offsets[$rotor]);
    }

    method bwd-pass(Int $rotor, Int $idx --> Int) {
        my $contact = mod26($idx + @!offsets[$rotor]);
        my $out = @BWD[@!rotors[$rotor]].substr($contact, 1).ord - 65;
        mod26($out - @!offsets[$rotor]);
    }

    method press-key(Str $c --> Str) {
        self.step();
        my $idx = @!plugboard[$c.ord - 65];
        $idx = self.fwd-pass(2, $idx);
        $idx = self.fwd-pass(1, $idx);
        $idx = self.fwd-pass(0, $idx);
        $idx = $REF.substr($idx, 1).ord - 65;
        $idx = self.bwd-pass(0, $idx);
        $idx = self.bwd-pass(1, $idx);
        $idx = self.bwd-pass(2, $idx);
        $idx = @!plugboard[$idx];
        chr(65 + $idx);
    }

    method encrypt(Str $text --> Str) {
        $text.uc.comb(/<[A..Z]>/).map({ self.press-key($_) }).join;
    }
}

# Test harness
say "Enigma Machine - Raku Implementation";
say "=====================================";

my @tests = (
    { rotors => (1,2,3), key => "AAA", plug => "", input => "AAAAA", expected => "BDZGO" },
    { rotors => (1,2,3), key => "AAA", plug => "", input => "HELLOWORLD", expected => "ILBDAAMTAZ" },
    { rotors => (1,2,3), key => "AAA", plug => "", input => "ATTACKATDAWN", expected => "BZHGNOCRRTCM" },
    { rotors => (1,2,3), key => "MCK", plug => "", input => "HELLOWORLD", expected => "DLTBBQVPQV" },
    { rotors => (3,1,2), key => "AAA", plug => "", input => "HELLOWORLD", expected => "KZHDFQYHXT" },
    { rotors => (1,2,3), key => "AAA", plug => "AB-CD-EF", input => "HELLOWORLD", expected => "IKACBBMTBF" },
);

my $pass = 0;
for @tests.kv -> $i, %t {
    my @r = %t<rotors>.list;
    my @k = %t<key>.comb;
    my $e = Enigma.new(r1 => @r[0], r2 => @r[1], r3 => @r[2],
                       k1 => @k[0], k2 => @k[1], k3 => @k[2], plug => %t<plug>);
    my $result = $e.encrypt(%t<input>);
    my $ok = $result eq %t<expected>;
    $pass++ if $ok;
    say "Test {$i+1}: %t<input> -> $result {$ok ?? '[PASS]' !! '[FAIL] expected %t<expected>'}";
}
say "\n$pass/6 tests passed";