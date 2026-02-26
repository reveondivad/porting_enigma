-- Enigma Cipher - SQL (PostgreSQL PL/pgSQL)
-- Implementing a cipher in a query language!

-- Rotor wiring tables
CREATE TABLE IF NOT EXISTS rotor_fwd (
    rotor_id INT,
    pos INT,
    val INT
);

CREATE TABLE IF NOT EXISTS rotor_bwd (
    rotor_id INT,
    pos INT,
    val INT
);

CREATE TABLE IF NOT EXISTS reflector (
    pos INT,
    val INT
);

CREATE TABLE IF NOT EXISTS notches (
    rotor_id INT,
    notch_pos INT
);

-- Insert Rotor I Forward
INSERT INTO rotor_fwd VALUES
(0,0,4),(0,1,10),(0,2,12),(0,3,5),(0,4,11),(0,5,6),(0,6,3),(0,7,16),
(0,8,21),(0,9,25),(0,10,13),(0,11,19),(0,12,14),(0,13,22),(0,14,24),
(0,15,7),(0,16,23),(0,17,20),(0,18,18),(0,19,15),(0,20,0),(0,21,8),
(0,22,1),(0,23,17),(0,24,2),(0,25,9);

-- Insert Rotor II Forward
INSERT INTO rotor_fwd VALUES
(1,0,0),(1,1,9),(1,2,3),(1,3,10),(1,4,18),(1,5,8),(1,6,17),(1,7,20),
(1,8,23),(1,9,1),(1,10,11),(1,11,7),(1,12,22),(1,13,19),(1,14,12),
(1,15,2),(1,16,16),(1,17,6),(1,18,25),(1,19,13),(1,20,15),(1,21,24),
(1,22,5),(1,23,21),(1,24,14),(1,25,4);

-- Insert Rotor III Forward
INSERT INTO rotor_fwd VALUES
(2,0,1),(2,1,3),(2,2,5),(2,3,7),(2,4,9),(2,5,11),(2,6,2),(2,7,15),
(2,8,17),(2,9,19),(2,10,23),(2,11,21),(2,12,25),(2,13,13),(2,14,24),
(2,15,4),(2,16,8),(2,17,22),(2,18,6),(2,19,0),(2,20,10),(2,21,12),
(2,22,20),(2,23,18),(2,24,16),(2,25,14);

-- Backward wirings
INSERT INTO rotor_bwd VALUES
(0,0,20),(0,1,22),(0,2,24),(0,3,6),(0,4,0),(0,5,3),(0,6,5),(0,7,15),
(0,8,21),(0,9,25),(0,10,1),(0,11,4),(0,12,2),(0,13,10),(0,14,12),
(0,15,19),(0,16,7),(0,17,23),(0,18,18),(0,19,11),(0,20,17),(0,21,8),
(0,22,13),(0,23,16),(0,24,14),(0,25,9);

INSERT INTO rotor_bwd VALUES
(1,0,0),(1,1,9),(1,2,15),(1,3,2),(1,4,25),(1,5,22),(1,6,17),(1,7,11),
(1,8,5),(1,9,1),(1,10,3),(1,11,10),(1,12,14),(1,13,19),(1,14,24),
(1,15,20),(1,16,16),(1,17,6),(1,18,4),(1,19,13),(1,20,7),(1,21,23),
(1,22,12),(1,23,8),(1,24,21),(1,25,18);

INSERT INTO rotor_bwd VALUES
(2,0,19),(2,1,0),(2,2,6),(2,3,1),(2,4,15),(2,5,2),(2,6,18),(2,7,3),
(2,8,16),(2,9,4),(2,10,20),(2,11,9),(2,12,21),(2,13,13),(2,14,25),
(2,15,7),(2,16,24),(2,17,8),(2,18,23),(2,19,5),(2,20,22),(2,21,11),
(2,22,17),(2,23,12),(2,24,14),(2,25,10);

INSERT INTO reflector VALUES
(0,24),(1,17),(2,20),(3,7),(4,16),(5,18),(6,11),(7,3),(8,15),(9,23),
(10,13),(11,6),(12,14),(13,10),(14,12),(15,8),(16,4),(17,1),(18,5),
(19,25),(20,2),(21,22),(22,21),(23,9),(24,0),(25,19);

INSERT INTO notches VALUES (0,16),(1,4),(2,21);

-- Enigma encrypt function
CREATE OR REPLACE FUNCTION enigma_encrypt(
    r0 INT, r1 INT, r2 INT,
    k0 INT, k1 INT, k2 INT,
    pb_pairs INT[][],
    msg TEXT
) RETURNS TEXT AS $$
DECLARE
    o0 INT := k0; o1 INT := k1; o2 INT := k2;
    result TEXT := '';
    i INT;
    ch INT;
    c INT;
    mid BOOLEAN;
    atn BOOLEAN;
    pb INT[26];
    inp INT;
    n0 INT; n1 INT; n2 INT;
BEGIN
    -- Init plugboard
    FOR i IN 0..25 LOOP pb[i+1] := i; END LOOP;
    IF pb_pairs IS NOT NULL THEN
        FOR i IN 1..array_length(pb_pairs, 1) LOOP
            pb[pb_pairs[i][1]+1] := pb_pairs[i][2];
            pb[pb_pairs[i][2]+1] := pb_pairs[i][1];
        END LOOP;
    END IF;

    -- Get notch positions
    SELECT notch_pos INTO n0 FROM notches WHERE rotor_id = r0;
    SELECT notch_pos INTO n1 FROM notches WHERE rotor_id = r1;
    SELECT notch_pos INTO n2 FROM notches WHERE rotor_id = r2;

    FOR i IN 1..length(msg) LOOP
        ch := ascii(substr(msg, i, 1)) - 65;

        -- Step rotors
        mid := (o1 = n1);
        atn := (o2 = n2);
        o2 := (o2 + 1) % 26;
        IF atn OR mid THEN o1 := (o1 + 1) % 26; END IF;
        IF mid THEN o0 := (o0 + 1) % 26; END IF;

        -- Plugboard in
        c := pb[ch + 1];

        -- Forward R2
        inp := (c + o2) % 26;
        SELECT val INTO c FROM rotor_fwd WHERE rotor_id = r2 AND pos = inp;
        c := ((c - o2) % 26 + 26) % 26;

        -- Forward R1
        inp := (c + o1) % 26;
        SELECT val INTO c FROM rotor_fwd WHERE rotor_id = r1 AND pos = inp;
        c := ((c - o1) % 26 + 26) % 26;

        -- Forward R0
        inp := (c + o0) % 26;
        SELECT val INTO c FROM rotor_fwd WHERE rotor_id = r0 AND pos = inp;
        c := ((c - o0) % 26 + 26) % 26;

        -- Reflector
        SELECT val INTO c FROM reflector WHERE pos = c;

        -- Backward R0
        inp := (c + o0) % 26;
        SELECT val INTO c FROM rotor_bwd WHERE rotor_id = r0 AND pos = inp;
        c := ((c - o0) % 26 + 26) % 26;

        -- Backward R1
        inp := (c + o1) % 26;
        SELECT val INTO c FROM rotor_bwd WHERE rotor_id = r1 AND pos = inp;
        c := ((c - o1) % 26 + 26) % 26;

        -- Backward R2
        inp := (c + o2) % 26;
        SELECT val INTO c FROM rotor_bwd WHERE rotor_id = r2 AND pos = inp;
        c := ((c - o2) % 26 + 26) % 26;

        -- Plugboard out
        c := pb[c + 1];
        result := result || chr(c + 65);
    END LOOP;

    RETURN result;
END;
$$ LANGUAGE plpgsql;

-- Run tests
SELECT 'Test 1: ' || enigma_encrypt(0,1,2, 0,0,0, NULL, 'AAAAA') || ' (expected BDZGO)';
SELECT 'Test 2: ' || enigma_encrypt(0,1,2, 0,0,0, NULL, 'HELLOWORLD') || ' (expected ILBDAAMTAZ)';
SELECT 'Test 3: ' || enigma_encrypt(0,1,2, 0,0,0, NULL, 'ATTACKATDAWN') || ' (expected BZHGNOCRRTCM)';
SELECT 'Test 4: ' || enigma_encrypt(0,1,2, 12,2,10, NULL, 'HELLOWORLD') || ' (expected DLTBBQVPQV)';
SELECT 'Test 5: ' || enigma_encrypt(2,0,1, 0,0,0, NULL, 'HELLOWORLD') || ' (expected KZHDFQYHXT)';
SELECT 'Test 6: ' || enigma_encrypt(0,1,2, 0,0,0, ARRAY[[0,1],[2,3],[4,5]], 'HELLOWORLD') || ' (expected IKACBBMTBF)';
