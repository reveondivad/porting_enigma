// Enigma cipher in SuperCollider (audio programming)
(
var rotorFwd = [
    [4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9],
    [0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4],
    [1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14]
];
var rotorBwd = [
    [20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9],
    [0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18],
    [19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12]
];
var reflector = [24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19];
var notches = [16, 4, 21];
var pos = [0, 0, 0];

var mod26 = { |n| ((n % 26) + 26) % 26 };
var rotorPass = { |wiring, c, p| mod26.value(wiring[mod26.value(c + p)] - p) };

var enigma = { |text|
    var result = "";
    pos = [0, 0, 0];
    text.toUpper.do { |ch|
        var c = ch.ascii - 65;
        if ((c >= 0) && (c < 26)) {
            var mid = pos[1] == notches[1];
            if (pos[2] == notches[2]) { pos[2] = mod26.value(pos[2] + 1) };
            if (mid || (pos[2] == notches[2])) { pos[1] = mod26.value(pos[1] + 1) };
            pos[2] = mod26.value(pos[2] + 1);
            [2,1,0].do { |i| c = rotorPass.value(rotorFwd[i], c, pos[i]) };
            c = reflector[c];
            [0,1,2].do { |i| c = rotorPass.value(rotorBwd[i], c, pos[i]) };
            result = result ++ (c + 65).asAscii;
        };
    };
    result
};

enigma.value("HELLOWORLD").postln;
)
