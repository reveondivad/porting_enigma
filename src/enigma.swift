// Enigma Machine — Rosetta Code Reference (Swift)
// Wehrmacht Enigma I: 3 rotors, Reflector B, plugboard.
// PeopleTec Inc. — Guinness World Record Attempt 2026

let FWD = ["EKMFLGDQVZNTOWYHXUSPAIBRCJ","AJDKSIRUXBLHWTMCQGZNPYFVOE","BDFHJLCPRTXVZNYEIWGAKMUSQO"]
let BWD = ["UWYGADFPVZBECKMTHXSLRINQOJ","AJPCZWRLFBDKOTYUQGENHXMIVS","TAGBPCSDQEUFVNZHYIXJWLRKOM"]
let NOTCH: [Int] = [Int(Character("Q").asciiValue!)-65, Int(Character("E").asciiValue!)-65, Int(Character("V").asciiValue!)-65]
let REFLECTOR = Array("YRUHQSLDPXNGOKMIEBFZCWVJAT")

func idx(_ c: Character) -> Int { Int(c.asciiValue!) - 65 }
func chr(_ i: Int) -> Character { Character(UnicodeScalar(i + 65)!) }

struct Enigma {
    var id: [Int], off: [Int], plug: [Int]

    init(_ rotors: [Int], _ key: String, _ plugboard: [String] = []) {
        id = rotors
        off = Array(key).map { idx($0) }
        plug = Array(0..<26)
        for p in plugboard {
            let chars = Array(p)
            let (a, b) = (idx(chars[0]), idx(chars[1]))
            plug[a] = b; plug[b] = a
        }
    }

    func fwd(_ r: Int, _ i: Int) -> Int {
        let c = (i + off[r]) % 26
        return (idx(Array(FWD[id[r]])[c]) - off[r] + 26) % 26
    }
    func bwd(_ r: Int, _ i: Int) -> Int {
        let c = (i + off[r]) % 26
        return (idx(Array(BWD[id[r]])[c]) - off[r] + 26) % 26
    }

    mutating func step() {
        if off[1] == NOTCH[id[1]] { off[1]=(off[1]+1)%26; off[0]=(off[0]+1)%26 }
        else if off[2] == NOTCH[id[2]] { off[1]=(off[1]+1)%26 }
        off[2]=(off[2]+1)%26
    }

    mutating func pressKey(_ ch: Character) -> Character {
        step()
        var i = idx(ch)
        i = plug[i]
        i = fwd(2,i); i = fwd(1,i); i = fwd(0,i)
        i = idx(REFLECTOR[i])
        i = bwd(0,i); i = bwd(1,i); i = bwd(2,i)
        i = plug[i]
        return chr(i)
    }

    mutating func encrypt(_ text: String) -> String {
        String(text.uppercased().filter{$0.isLetter}.map{pressKey($0)})
    }
}

let tests: [([Int],String,[String],String,String)] = [
    ([0,1,2],"AAA",[],"AAAAA","BDZGO"),
    ([0,1,2],"AAA",[],"HELLOWORLD","ILBDAAMTAZ"),
    ([0,1,2],"AAA",[],"ATTACKATDAWN","BZHGNOCRRTCM"),
    ([0,1,2],"MCK",[],"HELLOWORLD","DLTBBQVPQV"),
    ([2,0,1],"AAA",[],"HELLOWORLD","KZHDFQYHXT"),
    ([0,1,2],"AAA",["AB","CD","EF"],"HELLOWORLD","IKACBBMTBF"),
]

var allOk = true
for (t,(r,k,p,pt,exp)) in tests.enumerated() {
    var e = Enigma(r,k,p)
    let ct = e.encrypt(pt)
    let ok = ct == exp
    print("Test \(t+1): \(pt.padding(toLength:20,withPad:" ",startingAt:0)) -> \(ct.padding(toLength:15,withPad:" ",startingAt:0)) [\(ok ? "PASS" : "FAIL")]")
    if !ok { allOk = false }
}
print(allOk ? "\nALL 6 TESTS PASSED" : "\nSOME TESTS FAILED")
