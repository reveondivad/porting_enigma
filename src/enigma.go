// Enigma Machine — Rosetta Code Reference (Go)
// Wehrmacht Enigma I: 3 rotors, Reflector B, plugboard.
// PeopleTec Inc. — Guinness World Record Attempt 2026
package main

import (
	"fmt"
	"strings"
)

var (
	fwd  = [3]string{"EKMFLGDQVZNTOWYHXUSPAIBRCJ", "AJDKSIRUXBLHWTMCQGZNPYFVOE", "BDFHJLCPRTXVZNYEIWGAKMUSQO"}
	bwd  = [3]string{"UWYGADFPVZBECKMTHXSLRINQOJ", "AJPCZWRLFBDKOTYUQGENHXMIVS", "TAGBPCSDQEUFVNZHYIXJWLRKOM"}
	ntch = [3]byte{'Q', 'E', 'V'}
	refl = "YRUHQSLDPXNGOKMIEBFZCWVJAT"
)

type Enigma struct {
	id, off [3]int
	plug    [26]int
}

func NewEnigma(rotors [3]int, key string, plugs []string) *Enigma {
	e := &Enigma{id: rotors}
	for i := 0; i < 3; i++ { e.off[i] = int(key[i] - 'A') }
	for i := 0; i < 26; i++ { e.plug[i] = i }
	for _, p := range plugs {
		a, b := int(p[0]-'A'), int(p[1]-'A')
		e.plug[a], e.plug[b] = b, a
	}
	return e
}

func (e *Enigma) fwdPass(r, i int) int {
	c := (i + e.off[r]) % 26
	return (int(fwd[e.id[r]][c]-'A') - e.off[r] + 26) % 26
}
func (e *Enigma) bwdPass(r, i int) int {
	c := (i + e.off[r]) % 26
	return (int(bwd[e.id[r]][c]-'A') - e.off[r] + 26) % 26
}

func (e *Enigma) step() {
	if byte(e.off[1])+'A' == ntch[e.id[1]] {
		e.off[1] = (e.off[1] + 1) % 26
		e.off[0] = (e.off[0] + 1) % 26
	} else if byte(e.off[2])+'A' == ntch[e.id[2]] {
		e.off[1] = (e.off[1] + 1) % 26
	}
	e.off[2] = (e.off[2] + 1) % 26
}

func (e *Enigma) pressKey(ch byte) byte {
	e.step()
	i := int(ch - 'A')
	i = e.plug[i]
	i = e.fwdPass(2, i); i = e.fwdPass(1, i); i = e.fwdPass(0, i)
	i = int(refl[i] - 'A')
	i = e.bwdPass(0, i); i = e.bwdPass(1, i); i = e.bwdPass(2, i)
	i = e.plug[i]
	return byte(i) + 'A'
}

func (e *Enigma) Encrypt(text string) string {
	var sb strings.Builder
	for _, c := range strings.ToUpper(text) {
		if c >= 'A' && c <= 'Z' { sb.WriteByte(e.pressKey(byte(c))) }
	}
	return sb.String()
}

func main() {
	type tv struct{ r [3]int; k string; p []string; pt, exp string }
	tests := []tv{
		{[3]int{0,1,2},"AAA",nil,"AAAAA","BDZGO"},
		{[3]int{0,1,2},"AAA",nil,"HELLOWORLD","ILBDAAMTAZ"},
		{[3]int{0,1,2},"AAA",nil,"ATTACKATDAWN","BZHGNOCRRTCM"},
		{[3]int{0,1,2},"MCK",nil,"HELLOWORLD","DLTBBQVPQV"},
		{[3]int{2,0,1},"AAA",nil,"HELLOWORLD","KZHDFQYHXT"},
		{[3]int{0,1,2},"AAA",[]string{"AB","CD","EF"},"HELLOWORLD","IKACBBMTBF"},
	}
	allOk := true
	for i, t := range tests {
		e := NewEnigma(t.r, t.k, t.p)
		ct := e.Encrypt(t.pt)
		ok := ct == t.exp
		status := "PASS"; if !ok { status = "FAIL"; allOk = false }
		fmt.Printf("Test %d: %-20s -> %-15s [%s]\n", i+1, t.pt, ct, status)
	}
	if allOk { fmt.Println("\nALL 6 TESTS PASSED") } else { fmt.Println("\nSOME TESTS FAILED") }
}
