# Enigma Cipher - Hurl
# HTTP testing/scripting language
# Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
# PeopleTec Inc. - Guinness World Record Attempt 2026

# Enigma configuration as HTTP test assertions
# Rotor I: EKMFLGDQVZNTOWYHXUSPAIBRCJ -> [4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9]
# Rotor II: AJDKSIRUXBLHWTMCQGZNPYFVOE -> [0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4]
# Rotor III: BDFHJLCPRTXVZNYEIWGAKMUSQO -> [1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14]
# Reflector B: YRUHQSLDPXNGOKMIEBFZCWVJAT
# Notches: Q(16), E(4), V(21)

# Test encryption: AAAAA with rotors I,II,III and keys 0,0,0
GET http://localhost:8080/enigma/encrypt
[QueryStringParams]
rotors: 0,1,2
keys: 0,0,0
message: AAAAA

HTTP 200
[Asserts]
jsonpath "$.ciphertext" == "BDZGO"
jsonpath "$.rotors" == [0,1,2]
jsonpath "$.keys" == [0,0,0]

# Test encryption: HELLOWORLD
GET http://localhost:8080/enigma/encrypt
[QueryStringParams]
rotors: 0,1,2
keys: 0,0,0
message: HELLOWORLD

HTTP 200
[Asserts]
jsonpath "$.ciphertext" == "ILBDAAMTAZ"

# Test decryption (same settings decrypt ciphertext)
GET http://localhost:8080/enigma/encrypt
[QueryStringParams]
rotors: 0,1,2
keys: 0,0,0
message: BDZGO

HTTP 200
[Asserts]
jsonpath "$.ciphertext" == "AAAAA"
