// Enigma Cipher - Flix
// Functional-first language with Datalog on the JVM

def fwdI(): Array[Int32, Static] \ {} =
    Array#{4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9}

def fwdII(): Array[Int32, Static] \ {} =
    Array#{0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4}

def fwdIII(): Array[Int32, Static] \ {} =
    Array#{1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14}

def bwdI(): Array[Int32, Static] \ {} =
    Array#{20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9}

def bwdII(): Array[Int32, Static] \ {} =
    Array#{0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18}

def bwdIII(): Array[Int32, Static] \ {} =
    Array#{19,0,6,1,15,2,18,3,16,4,20,9,21,13,25,7,24,8,23,5,22,11,17,12,14,10}

def notches(): Array[Int32, Static] \ {} = Array#{16, 4, 21}

def reflectorB(): Array[Int32, Static] \ {} =
    Array#{24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19}

def mod26(x: Int32): Int32 = Int32.modulo(Int32.modulo(x, 26) + 26, 26)

def getFwd(r: Int32, i: Int32): Int32 =
    match r {
        case 0 => Array.get(i, fwdI())
        case 1 => Array.get(i, fwdII())
        case 2 => Array.get(i, fwdIII())
        case _ => 0
    }

def getBwd(r: Int32, i: Int32): Int32 =
    match r {
        case 0 => Array.get(i, bwdI())
        case 1 => Array.get(i, bwdII())
        case 2 => Array.get(i, bwdIII())
        case _ => 0
    }

def getNotch(r: Int32): Int32 = Array.get(r, notches())

def passForward(rotor: Int32, offset: Int32, ch: Int32): Int32 =
    let inp = mod26(ch + offset);
    let out = getFwd(rotor, inp);
    mod26(out - offset)

def passBackward(rotor: Int32, offset: Int32, ch: Int32): Int32 =
    let inp = mod26(ch + offset);
    let out = getBwd(rotor, inp);
    mod26(out - offset)

def makePlugboard(pairs: List[(Int32, Int32)]): Array[Int32, Static] \ {} =
    let pb = Array.init(Static, 26, i -> i);
    List.forEach(p -> {
        let (a, b) = p;
        Array.put(a, b, pb);
        Array.put(b, a, pb)
    }, pairs);
    pb

pub enum EnigmaState {
    case ES(Array[Int32, Static], Int32, Int32, Int32, Int32, Int32, Int32)
}

// rotors r0 r1 r2, offsets o0 o1 o2, plugboard
def encryptMsg(r0: Int32, r1: Int32, r2: Int32,
               o0: Int32, o1: Int32, o2: Int32,
               pb: Array[Int32, Static],
               chars: List[Int32]): List[Int32] =
    match chars {
        case Nil => Nil
        case ch :: rest =>
            let mid = (o1 == getNotch(r1));
            let atN2 = (o2 == getNotch(r2));
            let no2 = mod26(o2 + 1);
            let no1 = if (atN2 or mid) mod26(o1 + 1) else o1;
            let no0 = if (mid) mod26(o0 + 1) else o0;
            let c = Array.get(ch, pb);
            let c1 = passForward(r2, no2, c);
            let c2 = passForward(r1, no1, c1);
            let c3 = passForward(r0, no0, c2);
            let c4 = Array.get(c3, reflectorB());
            let c5 = passBackward(r0, no0, c4);
            let c6 = passBackward(r1, no1, c5);
            let c7 = passBackward(r2, no2, c6);
            let c8 = Array.get(c7, pb);
            c8 :: encryptMsg(r0, r1, r2, no0, no1, no2, pb, rest)
    }

def encrypt(r0: Int32, r1: Int32, r2: Int32,
            k0: Int32, k1: Int32, k2: Int32,
            pairs: List[(Int32, Int32)],
            msg: String): String =
    let pb = makePlugboard(pairs);
    let chars = String.toList(msg) |> List.map(c -> Char.toInt32(c) - 65);
    let result = encryptMsg(r0, r1, r2, k0, k1, k2, pb, chars);
    result |> List.map(i -> Char.fromInt32(i + 65)) |> List.toArray |> String.fromArray

def runTest(label: String, expected: String, actual: String): Unit \ IO =
    let status = if (expected == actual) "PASS" else "FAIL";
    println("${status} ${label}: ${actual} (expected ${expected})")

def main(): Unit \ IO =
    println("Enigma Cipher - Flix");
    runTest("Test 1", "BDZGO",
        encrypt(0,1,2, 0,0,0, Nil, "AAAAA"));
    runTest("Test 2", "ILBDAAMTAZ",
        encrypt(0,1,2, 0,0,0, Nil, "HELLOWORLD"));
    runTest("Test 3", "BZHGNOCRRTCM",
        encrypt(0,1,2, 0,0,0, Nil, "ATTACKATDAWN"));
    runTest("Test 4", "DLTBBQVPQV",
        encrypt(0,1,2, 12,2,10, Nil, "HELLOWORLD"));
    runTest("Test 5", "KZHDFQYHXT",
        encrypt(2,0,1, 0,0,0, Nil, "HELLOWORLD"));
    runTest("Test 6", "IKACBBMTBF",
        encrypt(0,1,2, 0,0,0, (0,1) :: (2,3) :: (4,5) :: Nil, "HELLOWORLD"))
