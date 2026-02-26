# Enigma cipher in NQP (Not Quite Perl - Raku bootstrapping)
my @rf1 := [4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9];
my @rf2 := [0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4];
my @rf3 := [1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14];
my @rb1 := [20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9];
my @rb2 := [0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18];
my @rb3 := [19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12];
my @ref := [24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19];
my @notch := [16, 4, 21];

sub mod26($n) { (($n % 26) + 26) % 26 }
sub rotor_pass(@w, $c, $p) { mod26(@w[mod26($c + $p)] - $p) }

sub enigma($text) {
    my @pos := [0, 0, 0];
    my $result := '';
    my $i := 0;
    while $i < nqp::chars($text) {
        my $ch := nqp::ord(nqp::uc(nqp::substr($text, $i, 1))) - 65;
        if $ch >= 0 && $ch < 26 {
            my $mid := @pos[1] == @notch[1];
            if @pos[2] == @notch[2] { @pos[2] := mod26(@pos[2] + 1) }
            if $mid || @pos[2] == @notch[2] { @pos[1] := mod26(@pos[1] + 1) }
            @pos[2] := mod26(@pos[2] + 1);
            my $c := $ch;
            $c := rotor_pass(@rf3, $c, @pos[2]);
            $c := rotor_pass(@rf2, $c, @pos[1]);
            $c := rotor_pass(@rf1, $c, @pos[0]);
            $c := @ref[$c];
            $c := rotor_pass(@rb1, $c, @pos[0]);
            $c := rotor_pass(@rb2, $c, @pos[1]);
            $c := rotor_pass(@rb3, $c, @pos[2]);
            $result := $result ~ nqp::chr($c + 65);
        }
        $i := $i + 1;
    }
    $result
}

say(enigma('HELLOWORLD'));
