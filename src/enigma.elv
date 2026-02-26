# Enigma cipher in Elvish shell
var rotor-fwd-0 = [4 10 12 5 11 6 3 16 21 25 13 19 14 22 24 7 23 20 18 15 0 8 1 17 2 9]
var rotor-fwd-1 = [0 9 3 10 18 8 17 20 23 1 11 7 22 19 12 2 16 6 25 13 15 24 5 21 14 4]
var rotor-fwd-2 = [1 3 5 7 9 11 2 15 17 19 23 21 25 13 24 4 8 22 6 0 10 12 20 18 16 14]
var rotor-bwd-0 = [20 22 24 6 0 3 5 15 21 25 1 4 2 10 12 19 7 23 18 11 17 8 13 16 14 9]
var rotor-bwd-1 = [0 9 15 2 25 22 17 11 5 1 3 10 14 19 24 20 16 6 4 13 7 23 12 8 21 18]
var rotor-bwd-2 = [19 0 6 1 15 2 18 3 16 4 20 5 21 13 25 7 24 8 23 9 22 11 17 10 14 12]
var reflector = [24 17 20 7 16 18 11 3 15 23 13 6 14 10 12 8 4 1 5 25 2 22 21 9 0 19]
var notches = [16 4 21]

fn mod26 {|n| put (% (+ (% $n 26) 26) 26) }

fn enigma {|text|
  var pos = [0 0 0]
  var result = ''
  each {|ch|
    var code = (- (ord $ch) 65)
    if (and (>= $code 0) (< $code 26)) {
      # Step rotors
      var mid = (== $pos[1] $notches[1])
      if (== $pos[2] $notches[2]) { set pos[2] = (mod26 (+ $pos[2] 1)) }
      if (or $mid (== $pos[2] $notches[2])) { set pos[1] = (mod26 (+ $pos[1] 1)) }
      set pos[2] = (mod26 (+ $pos[2] 1))
      var c = $code
      # Forward through rotors
      set c = (mod26 (- $rotor-fwd-2[(mod26 (+ $c $pos[2]))] $pos[2]))
      set c = (mod26 (- $rotor-fwd-1[(mod26 (+ $c $pos[1]))] $pos[1]))
      set c = (mod26 (- $rotor-fwd-0[(mod26 (+ $c $pos[0]))] $pos[0]))
      set c = $reflector[$c]
      set c = (mod26 (- $rotor-bwd-0[(mod26 (+ $c $pos[0]))] $pos[0]))
      set c = (mod26 (- $rotor-bwd-1[(mod26 (+ $c $pos[1]))] $pos[1]))
      set c = (mod26 (- $rotor-bwd-2[(mod26 (+ $c $pos[2]))] $pos[2]))
      set result = $result(chr (+ $c 65))
    }
  } [(str:split '' (str:to-upper $text))]
  echo $result
}

enigma HELLOWORLD
