-- Enigma Machine - Ada Implementation
-- Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping)
-- PeopleTec Inc. - Guinness World Record Attempt 2026

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;

procedure Enigma is

   subtype Letter is Integer range 0 .. 25;

   type Wiring is array (Letter) of Letter;
   type Plug_Array is array (Letter) of Letter;

   -- Rotor wirings (forward)
   FWD_I   : constant String := "EKMFLGDQVZNTOWYHXUSPAIBRCJ";
   FWD_II  : constant String := "AJDKSIRUXBLHWTMCQGZNPYFVOE";
   FWD_III : constant String := "BDFHJLCPRTXVZNYEIWGAKMUSQO";
   -- Rotor wirings (backward)
   BWD_I   : constant String := "UWYGADFPVZBECKMTHXSLRINQOJ";
   BWD_II  : constant String := "AJPCZWRLFBDKOTYUQGENHXMIVS";
   BWD_III : constant String := "TAGBPCSDQEUFVNZHYIXJWLRKOM";
   -- Reflector B
   REFL    : constant String := "YRUHQSLDPXNGOKMIEBFZCWVJAT";

   -- Notch positions: I=Q(16), II=E(4), III=V(21)
   Notch_I   : constant Letter := 16;
   Notch_II  : constant Letter := 4;
   Notch_III : constant Letter := 21;

   function Char_To_Idx (C : Character) return Letter is
   begin
      return Character'Pos (C) - Character'Pos ('A');
   end Char_To_Idx;

   function Idx_To_Char (I : Letter) return Character is
   begin
      return Character'Val (I + Character'Pos ('A'));
   end Idx_To_Char;

   function String_To_Wiring (S : String) return Wiring is
      W : Wiring;
   begin
      for I in Letter loop
         W (I) := Char_To_Idx (S (S'First + I));
      end loop;
      return W;
   end String_To_Wiring;

   function Modulo (A, B : Integer) return Integer is
      R : Integer := A mod B;
   begin
      if R < 0 then R := R + B; end if;
      return R;
   end Modulo;

   -- Rotor state
   type Rotor_Record is record
      Fwd   : Wiring;
      Bwd   : Wiring;
      Notch : Letter;
      Offset: Letter;
   end record;

   function Make_Rotor (Num : Integer; Win : Character) return Rotor_Record is
      R : Rotor_Record;
   begin
      case Num is
         when 1 => R.Fwd := String_To_Wiring (FWD_I);
                    R.Bwd := String_To_Wiring (BWD_I);
                    R.Notch := Notch_I;
         when 2 => R.Fwd := String_To_Wiring (FWD_II);
                    R.Bwd := String_To_Wiring (BWD_II);
                    R.Notch := Notch_II;
         when 3 => R.Fwd := String_To_Wiring (FWD_III);
                    R.Bwd := String_To_Wiring (BWD_III);
                    R.Notch := Notch_III;
         when others => null;
      end case;
      R.Offset := Char_To_Idx (Win);
      return R;
   end Make_Rotor;

   function Forward_Pass (R : Rotor_Record; Idx : Letter) return Letter is
      Contact : Letter := Modulo (Idx + R.Offset, 26);
   begin
      return Modulo (R.Fwd (Contact) - R.Offset, 26);
   end Forward_Pass;

   function Backward_Pass (R : Rotor_Record; Idx : Letter) return Letter is
      Contact : Letter := Modulo (Idx + R.Offset, 26);
   begin
      return Modulo (R.Bwd (Contact) - R.Offset, 26);
   end Backward_Pass;

   procedure Step_Rotor (R : in out Rotor_Record) is
   begin
      R.Offset := (R.Offset + 1) mod 26;
   end Step_Rotor;

   -- Enigma state
   Left, Middle, Right : Rotor_Record;
   Plug : Plug_Array;
   Reflector : Wiring;

   procedure Init_Enigma (R1, R2, R3 : Integer;
                          K1, K2, K3 : Character;
                          Plugboard  : String) is
      A, B : Letter;
      Pos  : Integer;
   begin
      Left   := Make_Rotor (R1, K1);
      Middle := Make_Rotor (R2, K2);
      Right  := Make_Rotor (R3, K3);
      Reflector := String_To_Wiring (REFL);
      for I in Letter loop
         Plug (I) := I;
      end loop;
      -- Parse plugboard pairs like "AB CD EF"
      Pos := Plugboard'First;
      while Pos + 1 <= Plugboard'Last loop
         if Plugboard (Pos) in 'A' .. 'Z' and Plugboard (Pos + 1) in 'A' .. 'Z' then
            A := Char_To_Idx (Plugboard (Pos));
            B := Char_To_Idx (Plugboard (Pos + 1));
            Plug (A) := B;
            Plug (B) := A;
            Pos := Pos + 2;
         else
            Pos := Pos + 1;
         end if;
      end loop;
   end Init_Enigma;

   procedure Step_Rotors is
   begin
      if Middle.Offset = Middle.Notch then
         Step_Rotor (Middle);
         Step_Rotor (Left);
      elsif Right.Offset = Right.Notch then
         Step_Rotor (Middle);
      end if;
      Step_Rotor (Right);
   end Step_Rotors;

   function Press_Key (C : Character) return Character is
      Idx : Letter := Char_To_Idx (C);
   begin
      Step_Rotors;
      Idx := Plug (Idx);
      Idx := Forward_Pass (Right, Idx);
      Idx := Forward_Pass (Middle, Idx);
      Idx := Forward_Pass (Left, Idx);
      Idx := Reflector (Idx);
      Idx := Backward_Pass (Left, Idx);
      Idx := Backward_Pass (Middle, Idx);
      Idx := Backward_Pass (Right, Idx);
      Idx := Plug (Idx);
      return Idx_To_Char (Idx);
   end Press_Key;

   function Encrypt (Text : String) return String is
      Result : String (1 .. Text'Length);
      Len    : Integer := 0;
      C      : Character;
   begin
      for I in Text'Range loop
         C := To_Upper (Text (I));
         if C in 'A' .. 'Z' then
            Len := Len + 1;
            Result (Len) := Press_Key (C);
         end if;
      end loop;
      return Result (1 .. Len);
   end Encrypt;

   -- Test harness
   type Test_Vector is record
      R1, R2, R3 : Integer;
      K          : String (1 .. 3);
      Plugs      : access String;
      Plain      : access String;
      Expected   : access String;
   end record;

   Empty  : aliased String := "";
   P1     : aliased String := "AAAAA";
   E1     : aliased String := "BDZGO";
   P2     : aliased String := "HELLOWORLD";
   E2     : aliased String := "ILBDAAMTAZ";
   P3     : aliased String := "ATTACKATDAWN";
   E3     : aliased String := "BZHGNOCRRTCM";
   P4     : aliased String := "HELLOWORLD";
   E4     : aliased String := "DLTBBQVPQV";
   P5     : aliased String := "HELLOWORLD";
   E5     : aliased String := "KZHDFQYHXT";
   PB6    : aliased String := "AB CD EF";
   P6     : aliased String := "HELLOWORLD";
   E6     : aliased String := "IKACBBMTBF";

   Tests : array (1 .. 6) of Test_Vector := (
      (1, 2, 3, "AAA", Empty'Access,  P1'Access, E1'Access),
      (1, 2, 3, "AAA", Empty'Access,  P2'Access, E2'Access),
      (1, 2, 3, "AAA", Empty'Access,  P3'Access, E3'Access),
      (1, 2, 3, "MCK", Empty'Access,  P4'Access, E4'Access),
      (3, 1, 2, "AAA", Empty'Access,  P5'Access, E5'Access),
      (1, 2, 3, "AAA", PB6'Access,    P6'Access, E6'Access)
   );

   All_Pass : Boolean := True;
   Cipher   : access String;

begin
   Put_Line ("Enigma Machine - Ada Implementation");
   Put_Line ("====================================");

   for I in Tests'Range loop
      Init_Enigma (Tests(I).R1, Tests(I).R2, Tests(I).R3,
                   Tests(I).K(1), Tests(I).K(2), Tests(I).K(3),
                   Tests(I).Plugs.all);
      declare
         C : String := Encrypt (Tests(I).Plain.all);
         Ok : Boolean := C = Tests(I).Expected.all;
      begin
         if Ok then
            Put_Line ("  Test" & Integer'Image(I) & ": " &
                      Tests(I).Plain.all & " -> " & C & " [PASS]");
         else
            Put_Line ("  Test" & Integer'Image(I) & ": " &
                      Tests(I).Plain.all & " -> " & C & " [FAIL] expected " &
                      Tests(I).Expected.all);
            All_Pass := False;
         end if;
      end;
   end loop;

   if All_Pass then
      Put_Line ("  ALL 6 TESTS PASSED");
   else
      Put_Line ("  SOME TESTS FAILED");
   end if;
end Enigma;
