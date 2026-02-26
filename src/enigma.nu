# Enigma cipher in Nushell
def mod26 [n: int] { (($n mod 26) + 26) mod 26 }

let rotor_fwd = [
  [4 10 12 5 11 6 3 16 21 25 13 19 14 22 24 7 23 20 18 15 0 8 1 17 2 9]
  [0 9 3 10 18 8 17 20 23 1 11 7 22 19 12 2 16 6 25 13 15 24 5 21 14 4]
  [1 3 5 7 9 11 2 15 17 19 23 21 25 13 24 4 8 22 6 0 10 12 20 18 16 14]
]
let rotor_bwd = [
  [20 22 24 6 0 3 5 15 21 25 1 4 2 10 12 19 7 23 18 11 17 8 13 16 14 9]
  [0 9 15 2 25 22 17 11 5 1 3 10 14 19 24 20 16 6 4 13 7 23 12 8 21 18]
  [19 0 6 1 15 2 18 3 16 4 20 5 21 13 25 7 24 8 23 9 22 11 17 10 14 12]
]
let reflector = [24 17 20 7 16 18 11 3 15 23 13 6 14 10 12 8 4 1 5 25 2 22 21 9 0 19]
let notches = [16 4 21]

def enigma [text: string] {
  mut pos = [0 0 0]
  mut result = ""
  let chars = ($text | str upcase | split chars)
  for ch in $chars {
    let code = ($ch | into int)
    if $code < 65 or $code > 90 { continue }
    let mid = ($pos | get 1) == ($notches | get 1)
    if ($pos | get 2) == ($notches | get 2) { $pos = ($pos | update 2 (mod26 (($pos | get 2) + 1))) }
    if $mid or ($pos | get 2) == ($notches | get 2) { $pos = ($pos | update 1 (mod26 (($pos | get 1) + 1))) }
    $pos = ($pos | update 2 (mod26 (($pos | get 2) + 1)))
    mut c = $code - 65
    for i in [2 1 0] {
      let entry = (mod26 ($c + ($pos | get $i)))
      $c = (mod26 (($rotor_fwd | get $i | get $entry) - ($pos | get $i)))
    }
    $c = ($reflector | get $c)
    for i in [0 1 2] {
      let entry = (mod26 ($c + ($pos | get $i)))
      $c = (mod26 (($rotor_bwd | get $i | get $entry) - ($pos | get $i)))
    }
    $result = $result + (char ($c + 65))
  }
  $result
}

print (enigma "HELLOWORLD")
