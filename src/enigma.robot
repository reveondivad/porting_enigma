*** Settings ***
Documentation    Enigma cipher in Robot Framework
Library          Collections
Library          String

*** Variables ***
@{RF1}    4    10    12    5    11    6    3    16    21    25    13    19    14    22    24    7    23    20    18    15    0    8    1    17    2    9
@{RF2}    0    9    3    10    18    8    17    20    23    1    11    7    22    19    12    2    16    6    25    13    15    24    5    21    14    4
@{RF3}    1    3    5    7    9    11    2    15    17    19    23    21    25    13    24    4    8    22    6    0    10    12    20    18    16    14
@{RB1}    20    22    24    6    0    3    5    15    21    25    1    4    2    10    12    19    7    23    18    11    17    8    13    16    14    9
@{RB2}    0    9    15    2    25    22    17    11    5    1    3    10    14    19    24    20    16    6    4    13    7    23    12    8    21    18
@{RB3}    19    0    6    1    15    2    18    3    16    4    20    5    21    13    25    7    24    8    23    9    22    11    17    10    14    12
@{REF}    24    17    20    7    16    18    11    3    15    23    13    6    14    10    12    8    4    1    5    25    2    22    21    9    0    19
@{NOTCHES}    16    4    21

*** Keywords ***
Mod26
    [Arguments]    ${n}
    ${m}=    Evaluate    (${n} % 26 + 26) % 26
    RETURN    ${m}

Rotor Pass
    [Arguments]    ${wiring}    ${c}    ${pos}
    ${idx}=    Mod26    ${c} + ${pos}
    ${val}=    Get From List    ${wiring}    ${idx}
    ${result}=    Mod26    ${val} - ${pos}
    RETURN    ${result}

Encrypt Enigma
    [Arguments]    ${text}
    ${upper}=    Convert To Upper Case    ${text}
    @{pos}=    Create List    ${0}    ${0}    ${0}
    ${result}=    Set Variable    ${EMPTY}
    FOR    ${ch}    IN    @{upper}
        ${code}=    Evaluate    ord("${ch}") - 65
        Continue For Loop If    ${code} < 0 or ${code} > 25
        ${c}=    Rotor Pass    ${RF3}    ${code}    ${pos}[2]
        ${c}=    Rotor Pass    ${RF2}    ${c}    ${pos}[1]
        ${c}=    Rotor Pass    ${RF1}    ${c}    ${pos}[0]
        ${c}=    Get From List    ${REF}    ${c}
        ${chr}=    Evaluate    chr(${c} + 65)
        ${result}=    Catenate    ${result}    ${chr}
    END
    Log    ${result}
    RETURN    ${result}

*** Test Cases ***
Encrypt Hello World
    ${result}=    Encrypt Enigma    HELLOWORLD
    Log    Enigma output: ${result}
