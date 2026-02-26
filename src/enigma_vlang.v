// Enigma Machine - V (Vlang) Implementation
// Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping)
// PeopleTec Inc. - Guinness World Record Attempt 2026

module main

const fwd = ['EKMFLGDQVZNTOWYHXUSPAIBRCJ', 'AJDKSIRUXBLHWTMCQGZNPYFVOE', 'BDFHJLCPRTXVZNYEIWGAKMUSQO']
const bwd = ['UWYGADFPVZBECKMTHXSLRINQOJ', 'AJPCZWRLFBDKOTYUQGENHXMIVS', 'TAGBPCSDQEUFVNZHYIXJWLRKOM']
const ref = 'YRUHQSLDPXNGOKMIEBFZCWVJAT'
const notch = [16, 4, 21]

struct Enigma {
mut:
	rotors   [3]int
	offsets  [3]int
	notches  [3]int
	plugboard [26]int
}

fn mod26(n int) int {
	return ((n % 26) + 26) % 26
}

fn new_enigma(r1 int, r2 int, r3 int, k1 u8, k2 u8, k3 u8, plug_pairs string) Enigma {
	mut e := Enigma{}
	e.rotors = [r1 - 1, r2 - 1, r3 - 1]!
	e.offsets = [int(k1 - `A`), int(k2 - `A`), int(k3 - `A`)]!
	e.notches = [notch[e.rotors[0]], notch[e.rotors[1]], notch[e.rotors[2]]]!
	for i in 0 .. 26 {
		e.plugboard[i] = i
	}
	if plug_pairs.len > 0 {
		for pair in plug_pairs.split('-') {
			a := int(pair[0] - `A`)
			b := int(pair[1] - `A`)
			e.plugboard[a] = b
			e.plugboard[b] = a
		}
	}
	return e
}

fn (mut e Enigma) step() {
	if e.offsets[1] == e.notches[1] {
		e.offsets[1] = mod26(e.offsets[1] + 1)
		e.offsets[0] = mod26(e.offsets[0] + 1)
	} else if e.offsets[2] == e.notches[2] {
		e.offsets[1] = mod26(e.offsets[1] + 1)
	}
	e.offsets[2] = mod26(e.offsets[2] + 1)
}

fn (e &Enigma) fwd_pass(rotor int, idx int) int {
	contact := mod26(idx + e.offsets[rotor])
	out := int(fwd[e.rotors[rotor]][contact] - `A`)
	return mod26(out - e.offsets[rotor])
}

fn (e &Enigma) bwd_pass(rotor int, idx int) int {
	contact := mod26(idx + e.offsets[rotor])
	out := int(bwd[e.rotors[rotor]][contact] - `A`)
	return mod26(out - e.offsets[rotor])
}

fn (mut e Enigma) press_key(c u8) u8 {
	e.step()
	mut idx := e.plugboard[int(c - `A`)]
	idx = e.fwd_pass(2, idx)
	idx = e.fwd_pass(1, idx)
	idx = e.fwd_pass(0, idx)
	idx = int(ref[idx] - `A`)
	idx = e.bwd_pass(0, idx)
	idx = e.bwd_pass(1, idx)
	idx = e.bwd_pass(2, idx)
	idx = e.plugboard[idx]
	return u8(65 + idx)
}

fn (mut e Enigma) encrypt(text string) string {
	upper := text.to_upper()
	mut result := []u8{}
	for c in upper {
		if c >= `A` && c <= `Z` {
			result << e.press_key(c)
		}
	}
	return result.bytestr()
}

fn main() {
	println('Enigma Machine - V Implementation')
	println('==================================')

	tests := [
		[[1,2,3], 'AAA', '', 'AAAAA', 'BDZGO'],
		[[1,2,3], 'AAA', '', 'HELLOWORLD', 'ILBDAAMTAZ'],
		[[1,2,3], 'AAA', '', 'ATTACKATDAWN', 'BZHGNOCRRTCM'],
		[[1,2,3], 'MCK', '', 'HELLOWORLD', 'DLTBBQVPQV'],
		[[3,1,2], 'AAA', '', 'HELLOWORLD', 'KZHDFQYHXT'],
		[[1,2,3], 'AAA', 'AB-CD-EF', 'HELLOWORLD', 'IKACBBMTBF'],
	]

	mut pass := 0
	for i, t in tests {
		mut e := new_enigma(t[0][0], t[0][1], t[0][2], t[1][0], t[1][1], t[1][2], t[2])
		result := e.encrypt(t[3])
		ok := result == t[4]
		if ok { pass++ }
		status := if ok { '[PASS]' } else { '[FAIL] expected ${t[4]}' }
		println('Test ${i+1}: ${t[3]} -> ${result} ${status}')
	}
	println('\n${pass}/6 tests passed')
}