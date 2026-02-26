// Enigma cipher data types in Apache Thrift (IDL)
namespace java enigma
namespace py enigma
namespace cpp enigma

struct RotorWiring {
  1: required list<i32> forward,
  2: required list<i32> backward,
  3: required i32 notch
}

struct EnigmaConfig {
  1: required RotorWiring rotor_i = {
    "forward": [4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9],
    "backward": [20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9],
    "notch": 16
  },
  2: required RotorWiring rotor_ii = {
    "forward": [0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4],
    "backward": [0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18],
    "notch": 4
  },
  3: required RotorWiring rotor_iii = {
    "forward": [1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14],
    "backward": [19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12],
    "notch": 21
  },
  4: required list<i32> reflector_b = [24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19]
}

struct EncryptRequest {
  1: required string plaintext,
  2: required list<i32> rotor_positions = [0, 0, 0],
  3: required list<i32> ring_settings = [0, 0, 0],
  4: optional map<i32, i32> plugboard
}

struct EncryptResponse {
  1: required string ciphertext,
  2: required list<i32> final_positions
}

service EnigmaService {
  EncryptResponse encrypt(1: EncryptRequest request),
  string getRotorWiring(1: i32 rotor_number)
}
