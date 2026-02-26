-- Enigma Cipher - CQL (Cassandra Query Language)
-- Storing Enigma config and test vectors in Cassandra tables
-- Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
-- PeopleTec Inc. - Guinness World Record Attempt 2026

CREATE KEYSPACE IF NOT EXISTS enigma WITH replication = {'class': 'SimpleStrategy', 'replication_factor': 1};
USE enigma;

CREATE TABLE IF NOT EXISTS rotor_wiring (
    rotor_id int,
    direction text,
    wiring list<int>,
    notch_position int,
    PRIMARY KEY (rotor_id, direction)
);

INSERT INTO rotor_wiring (rotor_id, direction, wiring, notch_position)
VALUES (0, 'forward', [4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9], 16);
INSERT INTO rotor_wiring (rotor_id, direction, wiring, notch_position)
VALUES (1, 'forward', [0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4], 4);
INSERT INTO rotor_wiring (rotor_id, direction, wiring, notch_position)
VALUES (2, 'forward', [1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14], 21);
INSERT INTO rotor_wiring (rotor_id, direction, wiring, notch_position)
VALUES (0, 'backward', [20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9], 16);
INSERT INTO rotor_wiring (rotor_id, direction, wiring, notch_position)
VALUES (1, 'backward', [0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18], 4);
INSERT INTO rotor_wiring (rotor_id, direction, wiring, notch_position)
VALUES (2, 'backward', [19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12], 21);

CREATE TABLE IF NOT EXISTS reflector (
    reflector_id text PRIMARY KEY,
    wiring list<int>
);

INSERT INTO reflector (reflector_id, wiring)
VALUES ('B', [24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19]);

CREATE TABLE IF NOT EXISTS test_vectors (
    test_id int PRIMARY KEY,
    rotors text,
    key_setting text,
    plaintext text,
    expected_ciphertext text
);

INSERT INTO test_vectors (test_id, rotors, key_setting, plaintext, expected_ciphertext)
VALUES (1, 'I-II-III', 'AAA', 'AAAAA', 'BDZGO');
INSERT INTO test_vectors (test_id, rotors, key_setting, plaintext, expected_ciphertext)
VALUES (2, 'I-II-III', 'AAA', 'HELLOWORLD', 'ILBDAAMTAZ');
INSERT INTO test_vectors (test_id, rotors, key_setting, plaintext, expected_ciphertext)
VALUES (3, 'I-II-III', 'AAA', 'ATTACKATDAWN', 'BZHGNOCRRTCM');
INSERT INTO test_vectors (test_id, rotors, key_setting, plaintext, expected_ciphertext)
VALUES (4, 'I-II-III', 'MCK', 'HELLOWORLD', 'DLTBBQVPQV');
INSERT INTO test_vectors (test_id, rotors, key_setting, plaintext, expected_ciphertext)
VALUES (5, 'III-I-II', 'AAA', 'HELLOWORLD', 'KZHDFQYHXT');

-- Query all test vectors
SELECT * FROM test_vectors;
