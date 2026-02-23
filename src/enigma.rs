// Enigma Machine — Rosetta Code Reference (Rust)
// Wehrmacht Enigma I: 3 rotors, Reflector B, plugboard.
// PeopleTec Inc. — Guinness World Record Attempt 2026

const FWD: [&[u8;26];3] = [b"EKMFLGDQVZNTOWYHXUSPAIBRCJ",b"AJDKSIRUXBLHWTMCQGZNPYFVOE",b"BDFHJLCPRTXVZNYEIWGAKMUSQO"];
const BWD: [&[u8;26];3] = [b"UWYGADFPVZBECKMTHXSLRINQOJ",b"AJPCZWRLFBDKOTYUQGENHXMIVS",b"TAGBPCSDQEUFVNZHYIXJWLRKOM"];
const NOTCH: [u8;3] = [b'Q', b'E', b'V'];
const REFLECTOR: &[u8;26] = b"YRUHQSLDPXNGOKMIEBFZCWVJAT";

fn idx(c: u8) -> usize { (c - b'A') as usize }

struct Enigma {
    id: [usize;3],
    offset: [usize;3],
    plug: [usize;26],
}

impl Enigma {
    fn new(rotors: [usize;3], key: &[u8;3], plugboard: &[&str]) -> Self {
        let mut plug = [0usize;26];
        for i in 0..26 { plug[i] = i; }
        for p in plugboard {
            let pb = p.as_bytes();
            let (a,b) = (idx(pb[0]), idx(pb[1]));
            plug[a] = b; plug[b] = a;
        }
        Enigma {
            id: rotors,
            offset: [idx(key[0]), idx(key[1]), idx(key[2])],
            plug,
        }
    }
    fn fwd(&self, r: usize, i: usize) -> usize {
        let c = (i + self.offset[r]) % 26;
        (idx(FWD[self.id[r]][c]) + 26 - self.offset[r]) % 26
    }
    fn bwd(&self, r: usize, i: usize) -> usize {
        let c = (i + self.offset[r]) % 26;
        (idx(BWD[self.id[r]][c]) + 26 - self.offset[r]) % 26
    }
    fn step(&mut self) {
        if (self.offset[1] as u8 + b'A') == NOTCH[self.id[1]] {
            self.offset[1] = (self.offset[1]+1)%26;
            self.offset[0] = (self.offset[0]+1)%26;
        } else if (self.offset[2] as u8 + b'A') == NOTCH[self.id[2]] {
            self.offset[1] = (self.offset[1]+1)%26;
        }
        self.offset[2] = (self.offset[2]+1)%26;
    }
    fn press_key(&mut self, ch: u8) -> u8 {
        self.step();
        let mut i = idx(ch);
        i = self.plug[i];
        i = self.fwd(2,i); i = self.fwd(1,i); i = self.fwd(0,i);
        i = idx(REFLECTOR[i]);
        i = self.bwd(0,i); i = self.bwd(1,i); i = self.bwd(2,i);
        i = self.plug[i];
        (i as u8) + b'A'
    }
    fn encrypt(&mut self, text: &str) -> String {
        text.to_uppercase().bytes()
            .filter(|&c| c >= b'A' && c <= b'Z')
            .map(|c| self.press_key(c) as char)
            .collect()
    }
}

fn main() {
    let tests: Vec<([usize;3],&[u8;3],Vec<&str>,&str,&str)> = vec![
        ([0,1,2],b"AAA",vec![],"AAAAA","BDZGO"),
        ([0,1,2],b"AAA",vec![],"HELLOWORLD","ILBDAAMTAZ"),
        ([0,1,2],b"AAA",vec![],"ATTACKATDAWN","BZHGNOCRRTCM"),
        ([0,1,2],b"MCK",vec![],"HELLOWORLD","DLTBBQVPQV"),
        ([2,0,1],b"AAA",vec![],"HELLOWORLD","KZHDFQYHXT"),
        ([0,1,2],b"AAA",vec!["AB","CD","EF"],"HELLOWORLD","IKACBBMTBF"),
    ];
    let mut all_ok = true;
    for (t,(rotors,key,plugs,plain,expected)) in tests.iter().enumerate() {
        let mut e = Enigma::new(*rotors, key, plugs);
        let ct = e.encrypt(plain);
        let ok = ct == *expected;
        println!("Test {}: {:20} -> {:15} [{}]", t+1, plain, ct, if ok {"PASS"} else {"FAIL"});
        if !ok { all_ok = false; }
    }
    println!("{}", if all_ok {"\nALL 6 TESTS PASSED"} else {"\nSOME TESTS FAILED"});
}
