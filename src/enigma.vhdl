-- Enigma Machine - VHDL Implementation (Simulation/Testbench)
-- Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping)
-- PeopleTec Inc. - Guinness World Record Attempt 2026
-- Note: This is a behavioral simulation model, not synthesizable RTL

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use STD.TEXTIO.ALL;

entity enigma_tb is
end enigma_tb;

architecture behavioral of enigma_tb is

    type int_array is array (0 to 25) of integer range 0 to 25;
    type rotor_set is array (1 to 3) of int_array;

    constant FWD : rotor_set := (
        1 => (4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9),
        2 => (0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4),
        3 => (1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14)
    );
    constant BWD : rotor_set := (
        1 => (20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9),
        2 => (0,9,15,2,25,22,17,11,3,1,5,10,18,13,24,16,20,6,4,23,7,21,12,8,14,19),
        3 => (19,0,6,1,15,2,18,3,16,4,20,5,21,13,8,7,24,9,23,10,22,11,17,12,14,25)
    );
    constant REF : int_array := (24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19);
    constant NOTCH : int_array := (16,4,21, others => 0);

    type enigma_state is record
        rotors   : integer range 1 to 3;
        offsets  : integer range 0 to 25;
        notches  : integer range 0 to 25;
    end record;

    -- Shared variables for simulation
    shared variable rotor_idx : array(0 to 2) of integer range 0 to 2;
    shared variable offsets : array(0 to 2) of integer range 0 to 25;
    shared variable notches : array(0 to 2) of integer range 0 to 25;
    shared variable plugboard : int_array;

    function mod26(n : integer) return integer is
        variable r : integer;
    begin
        r := n mod 26;
        if r < 0 then r := r + 26; end if;
        return r;
    end function;

    procedure init_enigma(r1, r2, r3 : integer; k1, k2, k3 : integer; plug_pairs : string) is
        variable a, b, p : integer;
    begin
        rotor_idx(0) := r1 - 1; rotor_idx(1) := r2 - 1; rotor_idx(2) := r3 - 1;
        offsets(0) := k1; offsets(1) := k2; offsets(2) := k3;
        notches(0) := NOTCH(rotor_idx(0));
        notches(1) := NOTCH(rotor_idx(1));
        notches(2) := NOTCH(rotor_idx(2));
        for i in 0 to 25 loop
            plugboard(i) := i;
        end loop;
        -- Parse plug pairs (simplified: expects "AB-CD-EF" format)
        p := plug_pairs'left;
        while p <= plug_pairs'right loop
            if plug_pairs(p) >= 'A' and plug_pairs(p) <= 'Z' and
               p + 1 <= plug_pairs'right and
               plug_pairs(p+1) >= 'A' and plug_pairs(p+1) <= 'Z' then
                a := character'pos(plug_pairs(p)) - character'pos('A');
                b := character'pos(plug_pairs(p+1)) - character'pos('A');
                plugboard(a) := b;
                plugboard(b) := a;
                p := p + 3;
            else
                p := p + 1;
            end if;
        end loop;
    end procedure;

    procedure step_rotors is
    begin
        if offsets(1) = notches(1) then
            offsets(1) := mod26(offsets(1) + 1);
            offsets(0) := mod26(offsets(0) + 1);
        elsif offsets(2) = notches(2) then
            offsets(1) := mod26(offsets(1) + 1);
        end if;
        offsets(2) := mod26(offsets(2) + 1);
    end procedure;

    function fwd_pass(rotor : integer; idx : integer) return integer is
        variable contact, out_val : integer;
    begin
        contact := mod26(idx + offsets(rotor));
        out_val := FWD(rotor_idx(rotor) + 1)(contact);
        return mod26(out_val - offsets(rotor));
    end function;

    function bwd_pass(rotor : integer; idx : integer) return integer is
        variable contact, out_val : integer;
    begin
        contact := mod26(idx + offsets(rotor));
        out_val := BWD(rotor_idx(rotor) + 1)(contact);
        return mod26(out_val - offsets(rotor));
    end function;

    function press_key(c : character) return character is
        variable idx : integer;
    begin
        step_rotors;
        idx := plugboard(character'pos(c) - character'pos('A'));
        idx := fwd_pass(2, idx);
        idx := fwd_pass(1, idx);
        idx := fwd_pass(0, idx);
        idx := REF(idx);
        idx := bwd_pass(0, idx);
        idx := bwd_pass(1, idx);
        idx := bwd_pass(2, idx);
        idx := plugboard(idx);
        return character'val(idx + character'pos('A'));
    end function;

    function encrypt(text : string) return string is
        variable result : string(1 to text'length);
        variable pos : integer := 0;
        variable c : character;
    begin
        for i in text'range loop
            c := text(i);
            if c >= 'a' and c <= 'z' then
                c := character'val(character'pos(c) - 32);
            end if;
            if c >= 'A' and c <= 'Z' then
                pos := pos + 1;
                result(pos) := press_key(c);
            end if;
        end loop;
        return result(1 to pos);
    end function;

begin
    process
        variable l : line;
        variable result : string(1 to 20);
        variable pass_count : integer := 0;

        procedure run_test(num : integer; r1,r2,r3,k1,k2,k3 : integer;
                          plug, input, expected : string) is
            variable res : string(1 to expected'length);
            variable ok : boolean;
        begin
            init_enigma(r1,r2,r3,k1,k2,k3, plug);
            res := encrypt(input);
            ok := res = expected;
            if ok then pass_count := pass_count + 1; end if;
            write(l, string'("Test "));
            write(l, num);
            write(l, string'(": "));
            write(l, input);
            write(l, string'(" -> "));
            write(l, res);
            if ok then write(l, string'(" [PASS]"));
            else write(l, string'(" [FAIL] expected ")); write(l, expected);
            end if;
            writeline(output, l);
        end procedure;

    begin
        write(l, string'("Enigma Machine - VHDL Implementation"));
        writeline(output, l);
        write(l, string'("====================================="));
        writeline(output, l);

        run_test(1, 1,2,3,0,0,0, "", "AAAAA", "BDZGO");
        run_test(2, 1,2,3,0,0,0, "", "HELLOWORLD", "ILBDAAMTAZ");
        run_test(3, 1,2,3,0,0,0, "", "ATTACKATDAWN", "BZHGNOCRRTCM");
        run_test(4, 1,2,3,12,2,10, "", "HELLOWORLD", "DLTBBQVPQV");
        run_test(5, 3,1,2,0,0,0, "", "HELLOWORLD", "KZHDFQYHXT");
        run_test(6, 1,2,3,0,0,0, "AB-CD-EF", "HELLOWORLD", "IKACBBMTBF");

        write(l, pass_count);
        write(l, string'("/6 tests passed"));
        writeline(output, l);
        wait;
    end process;
end behavioral;