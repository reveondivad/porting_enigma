// Enigma Machine - Scala Implementation
// Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping)
// PeopleTec Inc. - Guinness World Record Attempt 2026

object Enigma {
  val FWD = Map(
    1 -> "EKMFLGDQVZNTOWYHXUSPAIBRCJ",
    2 -> "AJDKSIRUXBLHWTMCQGZNPYFVOE",
    3 -> "BDFHJLCPRTXVZNYEIWGAKMUSQO"
  )
  val BWD = Map(
    1 -> "UWYGADFPVZBECKMTHXSLRINQOJ",
    2 -> "AJPCZWRLFBDKOTYUQGENHXMIVS",
    3 -> "TAGBPCSDQEUFVNZHYIXJWLRKOM"
  )
  val NOTCH = Map(1 -> 16, 2 -> 4, 3 -> 21)
  val REFL = "YRUHQSLDPXNGOKMIEBFZCWVJAT"

  def mod26(a: Int): Int = ((a % 26) + 26) % 26

  class Rotor(num: Int, win: Char) {
    val fwd = FWD(num)
    val bwd = BWD(num)
    val notch = NOTCH(num)
    var offset = win - 'A'

    def atNotch: Boolean = offset == notch
    def step(): Unit = offset = (offset + 1) % 26

    def forwardPass(idx: Int): Int = {
      val contact = mod26(idx + offset)
      mod26(fwd(contact) - 'A' - offset)
    }
    def backwardPass(idx: Int): Int = {
      val contact = mod26(idx + offset)
      mod26(bwd(contact) - 'A' - offset)
    }
  }

  class Machine(rotors: (Int, Int, Int), key: String, plugboard: List[String] = Nil) {
    val left = new Rotor(rotors._1, key(0))
    val middle = new Rotor(rotors._2, key(1))
    val right = new Rotor(rotors._3, key(2))
    val plug: Array[Int] = Array.tabulate(26)(identity)

    plugboard.foreach { pair =>
      val a = pair(0) - 'A'
      val b = pair(1) - 'A'
      plug(a) = b; plug(b) = a
    }

    private def stepRotors(): Unit = {
      if (middle.atNotch) { middle.step(); left.step() }
      else if (right.atNotch) { middle.step() }
      right.step()
    }

    def pressKey(c: Char): Char = {
      stepRotors()
      var idx = c - 'A'
      idx = plug(idx)
      idx = right.forwardPass(idx)
      idx = middle.forwardPass(idx)
      idx = left.forwardPass(idx)
      idx = REFL(idx) - 'A'
      idx = left.backwardPass(idx)
      idx = middle.backwardPass(idx)
      idx = right.backwardPass(idx)
      idx = plug(idx)
      ('A' + idx).toChar
    }

    def encrypt(text: String): String =
      text.toUpperCase.filter(_.isLetter).map(pressKey).mkString
  }

  def main(args: Array[String]): Unit = {
    println("Enigma Machine - Scala Implementation")
    println("======================================")

    val tests = List(
      ((1,2,3), "AAA", Nil,                     "AAAAA",        "BDZGO"),
      ((1,2,3), "AAA", Nil,                     "HELLOWORLD",   "ILBDAAMTAZ"),
      ((1,2,3), "AAA", Nil,                     "ATTACKATDAWN", "BZHGNOCRRTCM"),
      ((1,2,3), "MCK", Nil,                     "HELLOWORLD",   "DLTBBQVPQV"),
      ((3,1,2), "AAA", Nil,                     "HELLOWORLD",   "KZHDFQYHXT"),
      ((1,2,3), "AAA", List("AB","CD","EF"),     "HELLOWORLD",   "IKACBBMTBF")
    )

    var allPass = true
    tests.zipWithIndex.foreach { case ((rotors, key, plugs, plain, expected), i) =>
      val m = new Machine(rotors, key, plugs)
      val cipher = m.encrypt(plain)
      val ok = cipher == expected
      val status = if (ok) "PASS" else "FAIL"
      println(f"  Test ${i+1}: $plain%-20s -> $cipher%-15s [$status]")
      if (!ok) { println(s"          Expected $expected, got $cipher"); allPass = false }
    }
    println(if (allPass) "\n  ALL 6 TESTS PASSED" else "\n  SOME TESTS FAILED")
  }
}
