(* Enigma cipher in Ur/Web (statically-typed web language) *)
fun mod26 (n : int) : int =
  let val m = n mod 26
  in if m < 0 then m + 26 else m end

val rotorFwd1 : list int = 4 :: 10 :: 12 :: 5 :: 11 :: 6 :: 3 :: 16 :: 21 :: 25 :: 13 :: 19 :: 14 :: 22 :: 24 :: 7 :: 23 :: 20 :: 18 :: 15 :: 0 :: 8 :: 1 :: 17 :: 2 :: 9 :: []
val reflectorB : list int = 24 :: 17 :: 20 :: 7 :: 16 :: 18 :: 11 :: 3 :: 15 :: 23 :: 13 :: 6 :: 14 :: 10 :: 12 :: 8 :: 4 :: 1 :: 5 :: 25 :: 2 :: 22 :: 21 :: 9 :: 0 :: 19 :: []

fun nth (lst : list int) (n : int) : int =
  case lst of
      [] => 0
    | x :: rest => if n = 0 then x else nth rest (n - 1)

fun rotorPass (wiring : list int) (c : int) (pos : int) : int =
  mod26 (nth wiring (mod26 (c + pos)) - pos)

fun main () : transaction page =
  return <xml>
    <head><title>Enigma Ur/Web</title></head>
    <body>
      <h1>Wehrmacht Enigma I</h1>
      <p>Rotor I forward wiring loaded with {[List.length rotorFwd1]} elements</p>
      <p>Reflector B loaded with {[List.length reflectorB]} elements</p>
    </body>
  </xml>
