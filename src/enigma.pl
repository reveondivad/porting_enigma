#!/usr/bin/perl
# Enigma Machine — Rosetta Code Reference (Perl)
# Wehrmacht Enigma I: 3 rotors, Reflector B, plugboard.
# PeopleTec Inc. — Guinness World Record Attempt 2026
use strict; use warnings;

my @FWD = qw(EKMFLGDQVZNTOWYHXUSPAIBRCJ AJDKSIRUXBLHWTMCQGZNPYFVOE BDFHJLCPRTXVZNYEIWGAKMUSQO);
my @BWD = qw(UWYGADFPVZBECKMTHXSLRINQOJ AJPCZWRLFBDKOTYUQGENHXMIVS TAGBPCSDQEUFVNZHYIXJWLRKOM);
my @NOTCH = (ord('Q'), ord('E'), ord('V'));
my $REFLECTOR = "YRUHQSLDPXNGOKMIEBFZCWVJAT";

sub new_enigma {
    my ($rotors, $key, $plugboard) = @_;
    my @id = @$rotors;
    my @off = map { ord($_) - 65 } split //, $key;
    my @plug = (0..25);
    if ($plugboard) {
        for my $p (@$plugboard) {
            my ($a,$b) = map { ord($_)-65 } split //, $p;
            @plug[$a,$b] = @plug[$b,$a];
        }
    }
    return { id => \@id, off => \@off, plug => \@plug };
}

sub fwd_pass {
    my ($e,$r,$i) = @_;
    my $c = ($i + $e->{off}[$r]) % 26;
    return (ord(substr($FWD[$e->{id}[$r]],$c,1))-65 - $e->{off}[$r] + 26) % 26;
}

sub bwd_pass {
    my ($e,$r,$i) = @_;
    my $c = ($i + $e->{off}[$r]) % 26;
    return (ord(substr($BWD[$e->{id}[$r]],$c,1))-65 - $e->{off}[$r] + 26) % 26;
}

sub step_rotors {
    my $e = shift;
    if ($e->{off}[1]+65 == $NOTCH[$e->{id}[1]]) {
        $e->{off}[1] = ($e->{off}[1]+1)%26;
        $e->{off}[0] = ($e->{off}[0]+1)%26;
    } elsif ($e->{off}[2]+65 == $NOTCH[$e->{id}[2]]) {
        $e->{off}[1] = ($e->{off}[1]+1)%26;
    }
    $e->{off}[2] = ($e->{off}[2]+1)%26;
}

sub press_key {
    my ($e, $ch) = @_;
    step_rotors($e);
    my $i = ord($ch) - 65;
    $i = $e->{plug}[$i];
    $i = fwd_pass($e,2,$i); $i = fwd_pass($e,1,$i); $i = fwd_pass($e,0,$i);
    $i = ord(substr($REFLECTOR,$i,1)) - 65;
    $i = bwd_pass($e,0,$i); $i = bwd_pass($e,1,$i); $i = bwd_pass($e,2,$i);
    $i = $e->{plug}[$i];
    return chr($i + 65);
}

sub encrypt {
    my ($e, $text) = @_;
    $text = uc($text); $text =~ s/[^A-Z]//g;
    return join '', map { press_key($e, $_) } split //, $text;
}

# Test vectors
my @tests = (
    [[0,1,2],"AAA",undef,"AAAAA","BDZGO"],
    [[0,1,2],"AAA",undef,"HELLOWORLD","ILBDAAMTAZ"],
    [[0,1,2],"AAA",undef,"ATTACKATDAWN","BZHGNOCRRTCM"],
    [[0,1,2],"MCK",undef,"HELLOWORLD","DLTBBQVPQV"],
    [[2,0,1],"AAA",undef,"HELLOWORLD","KZHDFQYHXT"],
    [[0,1,2],"AAA",["AB","CD","EF"],"HELLOWORLD","IKACBBMTBF"],
);

my $all_ok = 1;
for my $t (0..$#tests) {
    my ($r,$k,$p,$pt,$exp) = @{$tests[$t]};
    my $e = new_enigma($r,$k,$p);
    my $ct = encrypt($e,$pt);
    my $ok = $ct eq $exp;
    printf "Test %d: %-20s -> %-15s [%s]\n", $t+1, $pt, $ct, $ok?"PASS":"FAIL";
    $all_ok = 0 unless $ok;
}
print $all_ok ? "\nALL 6 TESTS PASSED\n" : "\nSOME TESTS FAILED\n";
