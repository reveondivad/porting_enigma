{ Enigma Machine - Pascal Implementation }
{ Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping) }
{ PeopleTec Inc. - Guinness World Record Attempt 2026 }

program EnigmaMachine;

uses SysUtils;

const
  FWD: array[1..3] of string = (
    'EKMFLGDQVZNTOWYHXUSPAIBRCJ',
    'AJDKSIRUXBLHWTMCQGZNPYFVOE',
    'BDFHJLCPRTXVZNYEIWGAKMUSQO'
  );
  BWD: array[1..3] of string = (
    'UWYGADFPVZBECKMTHXSLRINQOJ',
    'AJPCZWRLFBDKOTYUQGENHXMIVS',
    'TAGBPCSDQEUFVNZHYIXJWLRKOM'
  );
  NOTCH: array[1..3] of integer = (16, 4, 21); { Q=16, E=4, V=21 }
  REFL: string = 'YRUHQSLDPXNGOKMIEBFZCWVJAT';

type
  TRotor = record
    fwd_w, bwd_w: string;
    notch, offset: integer;
  end;

var
  left_r, mid_r, right_r: TRotor;
  plug: array[0..25] of integer;

function Modp(a, b: integer): integer;
var r: integer;
begin
  r := a mod b;
  if r < 0 then r := r + b;
  Modp := r;
end;

procedure InitRotor(var R: TRotor; num: integer; win: char);
begin
  R.fwd_w := FWD[num];
  R.bwd_w := BWD[num];
  R.notch := NOTCH[num];
  R.offset := Ord(win) - Ord('A');
end;

function ForwardPass(var R: TRotor; idx: integer): integer;
var contact: integer;
begin
  contact := Modp(idx + R.offset, 26);
  ForwardPass := Modp(Ord(R.fwd_w[contact + 1]) - Ord('A') - R.offset, 26);
end;

function BackwardPass(var R: TRotor; idx: integer): integer;
var contact: integer;
begin
  contact := Modp(idx + R.offset, 26);
  BackwardPass := Modp(Ord(R.bwd_w[contact + 1]) - Ord('A') - R.offset, 26);
end;

procedure StepRotor(var R: TRotor);
begin
  R.offset := (R.offset + 1) mod 26;
end;

procedure InitEnigma(r1, r2, r3: integer; k1, k2, k3: char; plugboard: string);
var
  i, a, b: integer;
begin
  InitRotor(left_r, r1, k1);
  InitRotor(mid_r, r2, k2);
  InitRotor(right_r, r3, k3);
  for i := 0 to 25 do plug[i] := i;
  i := 1;
  while i < Length(plugboard) do begin
    if (plugboard[i] >= 'A') and (plugboard[i] <= 'Z') and
       (plugboard[i+1] >= 'A') and (plugboard[i+1] <= 'Z') then begin
      a := Ord(plugboard[i]) - Ord('A');
      b := Ord(plugboard[i+1]) - Ord('A');
      plug[a] := b;
      plug[b] := a;
      i := i + 2;
    end else
      i := i + 1;
  end;
end;

procedure StepRotors;
begin
  if mid_r.offset = mid_r.notch then begin
    StepRotor(mid_r);
    StepRotor(left_r);
  end else if right_r.offset = right_r.notch then
    StepRotor(mid_r);
  StepRotor(right_r);
end;

function PressKey(c: char): char;
var idx: integer;
begin
  StepRotors;
  idx := Ord(c) - Ord('A');
  idx := plug[idx];
  idx := ForwardPass(right_r, idx);
  idx := ForwardPass(mid_r, idx);
  idx := ForwardPass(left_r, idx);
  idx := Ord(REFL[idx + 1]) - Ord('A');
  idx := BackwardPass(left_r, idx);
  idx := BackwardPass(mid_r, idx);
  idx := BackwardPass(right_r, idx);
  idx := plug[idx];
  PressKey := Chr(idx + Ord('A'));
end;

function Encrypt(text: string): string;
var
  i: integer;
  c: char;
  result: string;
begin
  result := '';
  for i := 1 to Length(text) do begin
    c := UpCase(text[i]);
    if (c >= 'A') and (c <= 'Z') then
      result := result + PressKey(c);
  end;
  Encrypt := result;
end;

{ Test harness }
type
  TTestVec = record
    r1, r2, r3: integer;
    key: string;
    plugs: string;
    plain: string;
    expected: string;
  end;

var
  tests: array[1..6] of TTestVec;
  i: integer;
  cipher: string;
  allPass: boolean;

begin
  tests[1].r1:=1; tests[1].r2:=2; tests[1].r3:=3; tests[1].key:='AAA'; tests[1].plugs:=''; tests[1].plain:='AAAAA'; tests[1].expected:='BDZGO';
  tests[2].r1:=1; tests[2].r2:=2; tests[2].r3:=3; tests[2].key:='AAA'; tests[2].plugs:=''; tests[2].plain:='HELLOWORLD'; tests[2].expected:='ILBDAAMTAZ';
  tests[3].r1:=1; tests[3].r2:=2; tests[3].r3:=3; tests[3].key:='AAA'; tests[3].plugs:=''; tests[3].plain:='ATTACKATDAWN'; tests[3].expected:='BZHGNOCRRTCM';
  tests[4].r1:=1; tests[4].r2:=2; tests[4].r3:=3; tests[4].key:='MCK'; tests[4].plugs:=''; tests[4].plain:='HELLOWORLD'; tests[4].expected:='DLTBBQVPQV';
  tests[5].r1:=3; tests[5].r2:=1; tests[5].r3:=2; tests[5].key:='AAA'; tests[5].plugs:=''; tests[5].plain:='HELLOWORLD'; tests[5].expected:='KZHDFQYHXT';
  tests[6].r1:=1; tests[6].r2:=2; tests[6].r3:=3; tests[6].key:='AAA'; tests[6].plugs:='AB CD EF'; tests[6].plain:='HELLOWORLD'; tests[6].expected:='IKACBBMTBF';

  WriteLn('Enigma Machine - Pascal Implementation');
  WriteLn('=======================================');
  allPass := True;

  for i := 1 to 6 do begin
    InitEnigma(tests[i].r1, tests[i].r2, tests[i].r3,
               tests[i].key[1], tests[i].key[2], tests[i].key[3],
               tests[i].plugs);
    cipher := Encrypt(tests[i].plain);
    if cipher = tests[i].expected then
      WriteLn('  Test ', i, ': ', tests[i].plain, ' -> ', cipher, ' [PASS]')
    else begin
      WriteLn('  Test ', i, ': ', tests[i].plain, ' -> ', cipher, ' [FAIL] expected ', tests[i].expected);
      allPass := False;
    end;
  end;

  if allPass then WriteLn('  ALL 6 TESTS PASSED')
  else WriteLn('  SOME TESTS FAILED');
end.
