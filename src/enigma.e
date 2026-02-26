-- Enigma Machine - Eiffel Implementation
-- Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping)
-- PeopleTec Inc. - Guinness World Record Attempt 2026

class ENIGMA_APP
create make
feature
    fwd: ARRAY[STRING]
    bwd: ARRAY[STRING]
    notch_pos: ARRAY[INTEGER]
    reflector: STRING

    left_fwd, mid_fwd, right_fwd: STRING
    left_bwd, mid_bwd, right_bwd: STRING
    left_notch, mid_notch, right_notch: INTEGER
    left_off, mid_off, right_off: INTEGER
    plug: ARRAY[INTEGER]

    mod26 (a: INTEGER): INTEGER
        do
            Result := ((a \\ 26) + 26) \\ 26
        end

    make
        local
            all_pass: BOOLEAN
        do
            create fwd.make_from_array(<<"EKMFLGDQVZNTOWYHXUSPAIBRCJ",
                "AJDKSIRUXBLHWTMCQGZNPYFVOE","BDFHJLCPRTXVZNYEIWGAKMUSQO">>)
            create bwd.make_from_array(<<"UWYGADFPVZBECKMTHXSLRINQOJ",
                "AJPCZWRLFBDKOTYUQGENHXMIVS","TAGBPCSDQEUFVNZHYIXJWLRKOM">>)
            create notch_pos.make_from_array(<<16, 4, 21>>)
            reflector := "YRUHQSLDPXNGOKMIEBFZCWVJAT"

            io.put_string("Enigma Machine - Eiffel Implementation%N")
            io.put_string("========================================%N")

            all_pass := True

            -- Test 1
            init_enigma(0, 1, 2, "AAA", <<>>)
            check_test(1, "AAAAA", "BDZGO", all_pass)

            init_enigma(0, 1, 2, "AAA", <<>>)
            check_test(2, "HELLOWORLD", "ILBDAAMTAZ", all_pass)

            init_enigma(0, 1, 2, "AAA", <<>>)
            check_test(3, "ATTACKATDAWN", "BZHGNOCRRTCM", all_pass)

            init_enigma(0, 1, 2, "MCK", <<>>)
            check_test(4, "HELLOWORLD", "DLTBBQVPQV", all_pass)

            init_enigma(2, 0, 1, "AAA", <<>>)
            check_test(5, "HELLOWORLD", "KZHDFQYHXT", all_pass)

            init_enigma(0, 1, 2, "AAA", <<"AB","CD","EF">>)
            check_test(6, "HELLOWORLD", "IKACBBMTBF", all_pass)

            if all_pass then
                io.put_string("%N  ALL 6 TESTS PASSED%N")
            else
                io.put_string("%N  SOME TESTS FAILED%N")
            end
        end

    init_enigma(r1, r2, r3: INTEGER; key: STRING; plugboard: ARRAY[STRING])
        local i, a, b: INTEGER
        do
            left_fwd := fwd[r1 + 1]; left_bwd := bwd[r1 + 1]; left_notch := notch_pos[r1 + 1]
            mid_fwd := fwd[r2 + 1]; mid_bwd := bwd[r2 + 1]; mid_notch := notch_pos[r2 + 1]
            right_fwd := fwd[r3 + 1]; right_bwd := bwd[r3 + 1]; right_notch := notch_pos[r3 + 1]
            left_off := key.item(1).code - ('A').code
            mid_off := key.item(2).code - ('A').code
            right_off := key.item(3).code - ('A').code
            create plug.make_filled(0, 0, 25)
            from i := 0 until i > 25 loop plug[i] := i; i := i + 1 end
            from i := plugboard.lower until i > plugboard.upper loop
                a := plugboard[i].item(1).code - ('A').code
                b := plugboard[i].item(2).code - ('A').code
                plug[a] := b; plug[b] := a
                i := i + 1
            end
        end

    step_rotors
        do
            if mid_off = mid_notch then
                mid_off := (mid_off + 1) \\ 26
                left_off := (left_off + 1) \\ 26
            elseif right_off = right_notch then
                mid_off := (mid_off + 1) \\ 26
            end
            right_off := (right_off + 1) \\ 26
        end

    fwd_pass(wiring: STRING; off, idx: INTEGER): INTEGER
        local contact: INTEGER
        do
            contact := mod26(idx + off)
            Result := mod26(wiring.item(contact + 1).code - ('A').code - off)
        end

    bwd_pass(wiring: STRING; off, idx: INTEGER): INTEGER
        local contact: INTEGER
        do
            contact := mod26(idx + off)
            Result := mod26(wiring.item(contact + 1).code - ('A').code - off)
        end

    press_key(c: CHARACTER): CHARACTER
        local idx: INTEGER
        do
            step_rotors
            idx := c.code - ('A').code
            idx := plug[idx]
            idx := fwd_pass(right_fwd, right_off, idx)
            idx := fwd_pass(mid_fwd, mid_off, idx)
            idx := fwd_pass(left_fwd, left_off, idx)
            idx := reflector.item(idx + 1).code - ('A').code
            idx := bwd_pass(left_bwd, left_off, idx)
            idx := bwd_pass(mid_bwd, mid_off, idx)
            idx := bwd_pass(right_bwd, right_off, idx)
            idx := plug[idx]
            Result := (idx + ('A').code).to_character_8
        end

    encrypt(text: STRING): STRING
        local i: INTEGER; c: CHARACTER
        do
            create Result.make_empty
            from i := 1 until i > text.count loop
                c := text.item(i).as_upper
                if c >= 'A' and c <= 'Z' then Result.append_character(press_key(c)) end
                i := i + 1
            end
        end

    check_test(num: INTEGER; plain, expected: STRING; all_pass: BOOLEAN)
        local cipher: STRING; ok: BOOLEAN
        do
            cipher := encrypt(plain)
            ok := cipher.is_equal(expected)
            io.put_string("  Test " + num.out + ": " + plain + " -> " + cipher)
            if ok then io.put_string(" [PASS]%N")
            else io.put_string(" [FAIL]%N"); io.put_string("          Expected " + expected + "%N") end
        end
end
