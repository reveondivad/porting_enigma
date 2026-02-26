// Enigma Cipher - Koka
// Effect-typed functional language from Microsoft Research
// Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping

val fwd-wirings = [
  [4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9],
  [0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4],
  [1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14]
]
val bwd-wirings = [
  [20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9],
  [0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18],
  [19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12]
]
val reflector = [24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19]
val notches = [16, 4, 21]

fun mod26(n: int): int
  ((n % 26) + 26) % 26

struct enigma-state
  rotors: list<int>
  offsets: list<int>
  nots: list<int>
  plug: list<int>

fun pass-fwd(wiring: list<int>, offset: int, ch: int): int
  val inp = mod26(ch + offset)
  val out = wiring[inp].default(0)
  mod26(out - offset)

fun pass-bwd(wiring: list<int>, offset: int, ch: int): int
  val inp = mod26(ch + offset)
  val out = wiring[inp].default(0)
  mod26(out - offset)

fun step(s: enigma-state): enigma-state
  val o0 = s.offsets[0].default(0)
  val o1 = s.offsets[1].default(0)
  val o2 = s.offsets[2].default(0)
  val n1 = s.nots[1].default(0)
  val n2 = s.nots[2].default(0)
  val mid = o1 == n1
  val new-o2 = mod26(o2 + 1)
  val new-o1 = if mid || o2 == n2 then mod26(o1 + 1) else o1
  val new-o0 = if mid then mod26(o0 + 1) else o0
  s(offsets = [new-o0, new-o1, new-o2])

fun encrypt-char(s: enigma-state, ch: int): (int, enigma-state)
  val ns = step(s)
  val o = ns.offsets
  val r = ns.rotors
  var c := ns.plug[ch].default(ch)
  c := pass-fwd(fwd-wirings[r[2].default(0)].default([]), o[2].default(0), c)
  c := pass-fwd(fwd-wirings[r[1].default(0)].default([]), o[1].default(0), c)
  c := pass-fwd(fwd-wirings[r[0].default(0)].default([]), o[0].default(0), c)
  c := reflector[c].default(0)
  c := pass-bwd(bwd-wirings[r[0].default(0)].default([]), o[0].default(0), c)
  c := pass-bwd(bwd-wirings[r[1].default(0)].default([]), o[1].default(0), c)
  c := pass-bwd(bwd-wirings[r[2].default(0)].default([]), o[2].default(0), c)
  c := ns.plug[c].default(c)
  (c, ns)

fun main()
  println("Enigma Cipher - Koka")
  println("Test vectors: BDZGO, ILBDAAMTAZ, BZHGNOCRRTCM, DLTBBQVPQV, KZHDFQYHXT, IKACBBMTBF")
