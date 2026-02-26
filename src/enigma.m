% Enigma Machine - GNU Octave/MATLAB Implementation
% Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping)
% PeopleTec Inc. - Guinness World Record Attempt 2026

function enigma()
    fprintf('Enigma Machine - Octave/MATLAB Implementation\n');
    fprintf('==============================================\n');

    % Test vectors
    tests = {
        {[1,2,3], 'AAA', {},            'AAAAA',        'BDZGO'};
        {[1,2,3], 'AAA', {},            'HELLOWORLD',   'ILBDAAMTAZ'};
        {[1,2,3], 'AAA', {},            'ATTACKATDAWN', 'BZHGNOCRRTCM'};
        {[1,2,3], 'MCK', {},            'HELLOWORLD',   'DLTBBQVPQV'};
        {[3,1,2], 'AAA', {},            'HELLOWORLD',   'KZHDFQYHXT'};
        {[1,2,3], 'AAA', {'AB','CD','EF'}, 'HELLOWORLD', 'IKACBBMTBF'};
    };

    all_pass = true;
    for i = 1:length(tests)
        t = tests{i};
        rotors = t{1}; key = t{2}; plugs = t{3};
        plain = t{4}; expected = t{5};
        state = init_enigma(rotors, key, plugs);
        [~, cipher] = encrypt_text(state, plain);
        if strcmp(cipher, expected)
            fprintf('  Test %d: %-20s -> %-15s [PASS]\n', i, plain, cipher);
        else
            fprintf('  Test %d: %-20s -> %-15s [FAIL] expected %s\n', i, plain, cipher, expected);
            all_pass = false;
        end
    end
    if all_pass
        fprintf('\n  ALL 6 TESTS PASSED\n');
    else
        fprintf('\n  SOME TESTS FAILED\n');
    end
end

function state = init_enigma(rotors, key, plugboard)
    fwd{1} = 'EKMFLGDQVZNTOWYHXUSPAIBRCJ';
    fwd{2} = 'AJDKSIRUXBLHWTMCQGZNPYFVOE';
    fwd{3} = 'BDFHJLCPRTXVZNYEIWGAKMUSQO';
    bwd{1} = 'UWYGADFPVZBECKMTHXSLRINQOJ';
    bwd{2} = 'AJPCZWRLFBDKOTYUQGENHXMIVS';
    bwd{3} = 'TAGBPCSDQEUFVNZHYIXJWLRKOM';
    notch = [16, 4, 21]; % 0-based: Q=16, E=4, V=21
    refl = 'YRUHQSLDPXNGOKMIEBFZCWVJAT';

    state.left_fwd = fwd{rotors(1)};
    state.left_bwd = bwd{rotors(1)};
    state.left_notch = notch(rotors(1));
    state.left_off = key(1) - 'A';

    state.mid_fwd = fwd{rotors(2)};
    state.mid_bwd = bwd{rotors(2)};
    state.mid_notch = notch(rotors(2));
    state.mid_off = key(2) - 'A';

    state.right_fwd = fwd{rotors(3)};
    state.right_bwd = bwd{rotors(3)};
    state.right_notch = notch(rotors(3));
    state.right_off = key(3) - 'A';

    state.refl = refl;
    state.plug = 0:25;

    for k = 1:length(plugboard)
        pair = plugboard{k};
        a = pair(1) - 'A'; b = pair(2) - 'A';
        state.plug(a+1) = b; state.plug(b+1) = a;
    end
end

function r = mod26(a)
    r = mod(mod(a, 26) + 26, 26);
end

function [state, idx] = fwd_pass(state, wiring, offset, idx)
    contact = mod26(idx + offset);
    out = wiring(contact + 1) - 'A';
    idx = mod26(out - offset);
end

function [state, idx] = bwd_pass(state, wiring, offset, idx)
    contact = mod26(idx + offset);
    out = wiring(contact + 1) - 'A';
    idx = mod26(out - offset);
end

function state = step_rotors(state)
    if state.mid_off == state.mid_notch
        state.mid_off = mod(state.mid_off + 1, 26);
        state.left_off = mod(state.left_off + 1, 26);
    elseif state.right_off == state.right_notch
        state.mid_off = mod(state.mid_off + 1, 26);
    end
    state.right_off = mod(state.right_off + 1, 26);
end

function [state, out_char] = press_key(state, c)
    state = step_rotors(state);
    idx = c - 'A';
    idx = state.plug(idx + 1);
    [state, idx] = fwd_pass(state, state.right_fwd, state.right_off, idx);
    [state, idx] = fwd_pass(state, state.mid_fwd, state.mid_off, idx);
    [state, idx] = fwd_pass(state, state.left_fwd, state.left_off, idx);
    idx = state.refl(idx + 1) - 'A';
    [state, idx] = bwd_pass(state, state.left_bwd, state.left_off, idx);
    [state, idx] = bwd_pass(state, state.mid_bwd, state.mid_off, idx);
    [state, idx] = bwd_pass(state, state.right_bwd, state.right_off, idx);
    idx = state.plug(idx + 1);
    out_char = char(idx + 'A');
end

function [state, cipher] = encrypt_text(state, text)
    text = upper(text);
    cipher = '';
    for i = 1:length(text)
        if text(i) >= 'A' && text(i) <= 'Z'
            [state, c] = press_key(state, text(i));
            cipher = [cipher c];
        end
    end
end

enigma();
