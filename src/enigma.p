{ Enigma cipher in Pascal (classic Turbo Pascal style) }
program Enigma;

const
  RF1: array[0..25] of integer = (4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9);
  RF2: array[0..25] of integer = (0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4);
  RF3: array[0..25] of integer = (1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14);
  RB1: array[0..25] of integer = (20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9);
  RB2: array[0..25] of integer = (0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18);
  RB3: array[0..25] of integer = (19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12);
  REF: array[0..25] of integer = (24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19);
  NOTCH: array[0..2] of integer = (16, 4, 21);

var
  pos: array[0..2] of integer;

function Mod26(n: integer): integer;
var m: integer;
begin
  m := n mod 26;
  if m < 0 then m := m + 26;
  Mod26 := m;
end;

function RotorPass(var wiring: array of integer; c, p: integer): integer;
begin
  RotorPass := Mod26(wiring[Mod26(c + p)] - p);
end;

procedure EncryptEnigma(text: string);
var
  i, c, j: integer;
  mid: boolean;
  ch: char;
begin
  pos[0] := 0; pos[1] := 0; pos[2] := 0;
  for i := 1 to Length(text) do
  begin
    ch := UpCase(text[i]);
    c := Ord(ch) - 65;
    if (c < 0) or (c > 25) then continue;
    mid := pos[1] = NOTCH[1];
    if pos[2] = NOTCH[2] then pos[2] := Mod26(pos[2] + 1);
    if mid or (pos[2] = NOTCH[2]) then pos[1] := Mod26(pos[1] + 1);
    pos[2] := Mod26(pos[2] + 1);
    c := RotorPass(RF3, c, pos[2]);
    c := RotorPass(RF2, c, pos[1]);
    c := RotorPass(RF1, c, pos[0]);
    c := REF[c];
    c := RotorPass(RB1, c, pos[0]);
    c := RotorPass(RB2, c, pos[1]);
    c := RotorPass(RB3, c, pos[2]);
    Write(Chr(c + 65));
  end;
  WriteLn;
end;

begin
  EncryptEnigma('HELLOWORLD');
end.
