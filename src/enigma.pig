-- Enigma cipher in Apache Pig Latin (big data processing)
-- Conceptual: Pig is for data transformations, not general computation
-- This demonstrates Enigma rotor data as Pig relations

rotor_fwd_1 = LOAD 'rotor1.csv' USING PigStorage(',') AS (idx:int, val:int);
-- Data: 0,4  1,10  2,12  3,5  4,11  5,6  6,3  7,16  8,21  9,25
-- 10,13  11,19  12,14  13,22  14,24  15,7  16,23  17,20  18,18  19,15
-- 20,0  21,8  22,1  23,17  24,2  25,9

reflector_b = LOAD 'reflector.csv' USING PigStorage(',') AS (idx:int, val:int);
-- Data: 0,24  1,17  2,20  3,7  4,16  5,18  6,11  7,3  8,15  9,23
-- 10,13  11,6  12,14  13,10  14,12  15,8  16,4  17,1  18,5  19,25
-- 20,2  21,22  22,21  23,9  24,0  25,19

-- Apply rotor substitution via JOIN
input_chars = LOAD 'input.csv' USING PigStorage(',') AS (pos:int, char_val:int);

-- Forward pass through Rotor I
rotor1_result = JOIN input_chars BY char_val, rotor_fwd_1 BY idx;
forward_pass = FOREACH rotor1_result GENERATE input_chars::pos AS pos, rotor_fwd_1::val AS char_val;

-- Reflector
reflected = JOIN forward_pass BY char_val, reflector_b BY idx;
ref_result = FOREACH reflected GENERATE forward_pass::pos AS pos, reflector_b::val AS char_val;

STORE ref_result INTO 'enigma_output';
