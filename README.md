Enigma Polyglot
A world-scale demonstration of software polyglotism

Proposed Guinness World Record title:
Most programming languages used to implement the same working computer program

Claim (current estimate):
513 distinct programming languages
620 independently written source files

What this is
This repository expresses the same working software system across hundreds of programming languages: a complete Enigma rotor cipher simulation.
It is not a “Hello World” collection. Each implementation performs stateful rotor stepping, reflector mapping, optional plugboard permutations, and reversible encryption/decryption.

Why Enigma
Enigma is complex enough to be meaningful:
- state changes every character (rotor stepping)
- deterministic and repeatable
- encryption is its own inverse under the same configuration
- historically significant and widely understood

Canonical reference
Behavior is anchored to the publicly available NSA Enigma simulator reference implementation:
https://github.com/NationalSecurityAgency/enigma-simulator

How the record is measured
The measurable unit is the number of distinct programming languages with a working Enigma implementation.
We report:
- File count (supporting evidence): 620
- Distinct language count (record unit): 513

Why 513 and not 620
Some files are deliberate variants of the same language (for example: pony3…pony9, groovy2, zig2, vale10…vale13, and enigma_2.* duplicates).
For Guinness-style “one variable” measurement, these are normalized to a single language.

Where the proof lives
- docs/RECORD_PROPOSAL.txt (record definition + counting rules)
- docs/TEST_VECTORS.txt (fixed pass/fail test vectors)
- stats/COUNTS.txt (generated counts)
- evidence/MANIFEST.csv (generated manifest: file -> normalized language)
- verification/ (scripts + logs)

How to regenerate the evidence
Run the manifest generator described in verification/GENERATE_MANIFEST.txt

Summary
One program. Hundreds of languages. Identical behavior.
