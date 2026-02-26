(: Enigma Cipher - XQuery 3.1
   Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
   PeopleTec Inc. - Guinness World Record Attempt 2026
   Usage: Run with Saxon or BaseX :)

declare variable $ALPHA := "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
declare variable $FWD := ("EKMFLGDQVZNTOWYHXUSPAIBRCJ","AJDKSIRUXBLHWTMCQGZNPYFVOE","BDFHJLCPRTXVZNYEIWGAKMUSQO");
declare variable $BWD := ("UWYGADFPVZBECKMTHXSLRINQOJ","AJPCZWRLFBDKOTYUQGENHXMIVS","TAGBPCSDQEUFVNZHYIXJWLRKOM");
declare variable $REF := "YRUHQSLDPXNGOKMIEBFZCWVJAT";
declare variable $NOTCH := (16, 4, 21);

declare function local:c2i($c as xs:string) as xs:integer {
  string-length(substring-before($ALPHA, $c))
};

declare function local:i2c($i as xs:integer) as xs:string {
  substring($ALPHA, $i + 1, 1)
};

declare function local:mod26($n as xs:integer) as xs:integer {
  (($n mod 26) + 26) mod 26
};

declare function local:fwd-pass($ch as xs:integer, $wiring as xs:string, $offset as xs:integer) as xs:integer {
  let $inp := local:mod26($ch + $offset)
  let $out := local:c2i(substring($wiring, $inp + 1, 1))
  return local:mod26($out - $offset)
};

declare function local:bwd-pass($ch as xs:integer, $wiring as xs:string, $offset as xs:integer) as xs:integer {
  let $inp := local:mod26($ch + $offset)
  let $c := local:i2c($inp)
  let $pos := string-length(substring-before($wiring, $c))
  return local:mod26($pos - $offset)
};

declare function local:encrypt-recursive(
  $chars as xs:integer*,
  $pos as xs:integer,
  $rR as xs:string, $rM as xs:string, $rL as xs:string,
  $oR as xs:integer, $oM as xs:integer, $oL as xs:integer,
  $nR as xs:integer, $nM as xs:integer
) as xs:string {
  if ($pos > count($chars)) then ""
  else
    let $midAtNotch := $oM eq $nM
    let $rAtNotch := $oR eq $nR
    let $newR := local:mod26($oR + 1)
    let $newM := if ($midAtNotch or $rAtNotch) then local:mod26($oM + 1) else $oM
    let $newL := if ($midAtNotch) then local:mod26($oL + 1) else $oL
    let $ch := $chars[$pos]
    let $c1 := local:fwd-pass($ch, $rR, $newR)
    let $c2 := local:fwd-pass($c1, $rM, $newM)
    let $c3 := local:fwd-pass($c2, $rL, $newL)
    let $c4 := local:c2i(substring($REF, $c3 + 1, 1))
    let $c5 := local:bwd-pass($c4, $rL, $newL)
    let $c6 := local:bwd-pass($c5, $rM, $newM)
    let $c7 := local:bwd-pass($c6, $rR, $newR)
    return concat(
      local:i2c($c7),
      local:encrypt-recursive($chars, $pos + 1, $rR, $rM, $rL, $newR, $newM, $newL, $nR, $nM)
    )
};

declare function local:enigma($rotors as xs:integer*, $key as xs:integer*, $text as xs:string) as xs:string {
  let $rL := $FWD[$rotors[1]]
  let $rM := $FWD[$rotors[2]]
  let $rR := $FWD[$rotors[3]]
  let $nM := $NOTCH[$rotors[2]]
  let $nR := $NOTCH[$rotors[3]]
  let $chars := for $i in 1 to string-length($text)
                return local:c2i(substring($text, $i, 1))
  return local:encrypt-recursive($chars, 1, $rR, $rM, $rL, $key[3], $key[2], $key[1], $nR, $nM)
};

<results>
  <test n="1">{local:enigma((1,2,3), (0,0,0), "AAAAA")}</test>
  <test n="2">{local:enigma((1,2,3), (0,0,0), "HELLOWORLD")}</test>
  <test n="3">{local:enigma((1,2,3), (0,0,0), "ATTACKATDAWN")}</test>
  <test n="4">{local:enigma((1,2,3), (12,2,10), "HELLOWORLD")}</test>
  <test n="5">{local:enigma((3,1,2), (0,0,0), "HELLOWORLD")}</test>
</results>
