-- Enigma Cipher - Ada 2012
-- Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping)
-- PeopleTec Inc. - Guinness World Record Attempt 2026
-- Note: Distinct from GNAT Ada (enigma.adb) - this uses Ada 2012 features

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Enigma_Ada2012 is

   type Wiring is array (0 .. 25) of Integer;
   type Rotor_Set is array (1 .. 3) of Wiring;

   FWD : constant Rotor_Set := (
     1 => (4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9),
     2 => (0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4),
     3 => (1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14));
   BWD : constant Rotor_Set := (
     1 => (20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9),
     2 => (0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18),
     3 => (19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12));
   REF : constant Wiring := (24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19);
   NOTCH : constant array (1 .. 3) of Integer := (16, 4, 21);

   type Enigma_State is record
      Rotors  : array (1 .. 3) of Integer;
      Offsets : array (1 .. 3) of Integer;
      Notches : array (1 .. 3) of Integer;
      Plug    : Wiring;
   end record;

   function Mod26 (N : Integer) return Integer is
      M : Integer := N mod 26;
   begin
      if M < 0 then M := M + 26; end if;
      return M;
   end Mod26;

   function Fwd_Pass (S : Enigma_State; Slot, Idx : Integer) return Integer is
      Contact : constant Integer := Mod26 (Idx + S.Offsets (Slot));
      Output  : constant Integer := FWD (S.Rotors (Slot))(Contact);
   begin
      return Mod26 (Output - S.Offsets (Slot));
   end Fwd_Pass;

   function Bwd_Pass (S : Enigma_State; Slot, Idx : Integer) return Integer is
      Contact : constant Integer := Mod26 (Idx + S.Offsets (Slot));
      Output  : constant Integer := BWD (S.Rotors (Slot))(Contact);
   begin
      return Mod26 (Output - S.Offsets (Slot));
   end Bwd_Pass;

   procedure Step (S : in out Enigma_State) is
   begin
      if S.Offsets (2) = S.Notches (2) then
         S.Offsets (2) := Mod26 (S.Offsets (2) + 1);
         S.Offsets (1) := Mod26 (S.Offsets (1) + 1);
      elsif S.Offsets (3) = S.Notches (3) then
         S.Offsets (2) := Mod26 (S.Offsets (2) + 1);
      end if;
      S.Offsets (3) := Mod26 (S.Offsets (3) + 1);
   end Step;

   function Press_Key (S : in out Enigma_State; Ch : Integer) return Integer is
      C : Integer;
   begin
      Step (S);
      C := S.Plug (Ch);
      C := Fwd_Pass (S, 3, C); C := Fwd_Pass (S, 2, C); C := Fwd_Pass (S, 1, C);
      C := REF (C);
      C := Bwd_Pass (S, 1, C); C := Bwd_Pass (S, 2, C); C := Bwd_Pass (S, 3, C);
      return S.Plug (C);
   end Press_Key;

   function Encrypt (R1, R2, R3, K1, K2, K3 : Integer; Msg : String) return String is
      S : Enigma_State;
      Result : Unbounded_String := Null_Unbounded_String;
   begin
      S.Rotors := (R1, R2, R3); S.Offsets := (K1, K2, K3);
      S.Notches := (NOTCH(R1), NOTCH(R2), NOTCH(R3));
      for I in 0 .. 25 loop S.Plug(I) := I; end loop;
      for I in Msg'Range loop
         declare
            Ch : constant Integer := Character'Pos(Msg(I)) - 65;
            Enc : constant Integer := Press_Key(S, Ch);
         begin
            Append(Result, Character'Val(Enc + 65));
         end;
      end loop;
      return To_String(Result);
   end Encrypt;

   procedure Run_Test (N : Integer; Exp, Res : String) is
   begin
      Put_Line("Test" & Integer'Image(N) & ": " & Res &
               (if Res = Exp then " [PASS]" else " [FAIL] expected " & Exp));
   end Run_Test;

begin
   Put_Line("Enigma Cipher - Ada 2012");
   Run_Test(1, "BDZGO", Encrypt(1,2,3,0,0,0,"AAAAA"));
   Run_Test(2, "ILBDAAMTAZ", Encrypt(1,2,3,0,0,0,"HELLOWORLD"));
   Run_Test(3, "BZHGNOCRRTCM", Encrypt(1,2,3,0,0,0,"ATTACKATDAWN"));
   Run_Test(4, "DLTBBQVPQV", Encrypt(1,2,3,12,2,10,"HELLOWORLD"));
   Run_Test(5, "KZHDFQYHXT", Encrypt(3,1,2,0,0,0,"HELLOWORLD"));
end Enigma_Ada2012;
