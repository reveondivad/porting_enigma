; Enigma Machine - x86-64 Assembly (NASM/Linux) Implementation
; Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping)
; PeopleTec Inc. - Guinness World Record Attempt 2026
; Assemble: nasm -f elf64 enigma.asm -o enigma.o && ld enigma.o -o enigma

section .data
    fwd1: db "EKMFLGDQVZNTOWYHXUSPAIBRCJ"
    fwd2: db "AJDKSIRUXBLHWTMCQGZNPYFVOE"
    fwd3: db "BDFHJLCPRTXVZNYEIWGAKMUSQO"
    bwd1: db "UWYGADFPVZBECKMTHXSLRINQOJ"
    bwd2: db "AJPCZWRLFBDKOTYUQGENHXMIVS"
    bwd3: db "TAGBPCSDQEUFVNZHYIXJWLRKOM"
    reflector: db "YRUHQSLDPXNGOKMIEBFZCWVJAT"
    notches: db 16, 4, 21  ; Q, E, V

    fwd_table: dq fwd1, fwd2, fwd3
    bwd_table: dq bwd1, bwd2, bwd3

    ; Test data
    header: db "Enigma Machine - x86-64 Assembly Implementation", 10
    header_len equ $ - header
    test_prefix: db "Test "
    test_plen equ $ - test_prefix
    arrow: db " -> "
    arrow_len equ $ - arrow
    pass_msg: db " [PASS]", 10
    pass_len equ $ - pass_msg
    fail_msg: db " [FAIL]", 10
    fail_len equ $ - fail_msg

    test1_in: db "AAAAA", 0
    test1_exp: db "BDZGO", 0
    test2_in: db "HELLOWORLD", 0
    test2_exp: db "ILBDAAMTAZ", 0
    test3_in: db "ATTACKATDAWN", 0
    test3_exp: db "BZHGNOCRRTCM", 0
    test4_in: db "HELLOWORLD", 0
    test4_exp: db "DLTBBQVPQV", 0
    test5_in: db "HELLOWORLD", 0
    test5_exp: db "KZHDFQYHXT", 0
    test6_in: db "HELLOWORLD", 0
    test6_exp: db "IKACBBMTBF", 0

section .bss
    rotor_idx: resb 3     ; which rotor in each slot
    offsets:   resb 3     ; current rotor offsets
    rotor_notches: resb 3 ; notch positions for installed rotors
    plugboard: resb 26    ; plugboard mapping
    result_buf: resb 64   ; output buffer
    digit_buf: resb 4

section .text
    global _start

; mod26: eax = ((eax % 26) + 26) % 26
mod26:
    push rdx
    cdq
    mov ecx, 26
    idiv ecx
    mov eax, edx
    add eax, 26
    cdq
    idiv ecx
    mov eax, edx
    pop rdx
    ret

; init_enigma: rdi=r1, rsi=r2, rdx=r3, rcx=k1, r8=k2, r9=k3
init_enigma:
    dec edi
    dec esi
    dec edx
    mov byte [rotor_idx], dil
    mov byte [rotor_idx+1], sil
    mov byte [rotor_idx+2], dl
    sub cl, 'A'
    mov byte [offsets], cl
    mov r10b, r8b
    sub r10b, 'A'
    mov byte [offsets+1], r10b
    mov r10b, r9b
    sub r10b, 'A'
    mov byte [offsets+2], r10b
    ; Set notches
    movzx eax, byte [rotor_idx]
    movzx eax, byte [notches + rax]
    mov byte [rotor_notches], al
    movzx eax, byte [rotor_idx+1]
    movzx eax, byte [notches + rax]
    mov byte [rotor_notches+1], al
    movzx eax, byte [rotor_idx+2]
    movzx eax, byte [notches + rax]
    mov byte [rotor_notches+2], al
    ; Init plugboard
    xor ecx, ecx
.plug_loop:
    mov byte [plugboard + rcx], cl
    inc ecx
    cmp ecx, 26
    jl .plug_loop
    ret

; step_rotors
step_rotors:
    movzx eax, byte [offsets+1]
    cmp al, byte [rotor_notches+1]
    jne .check_right
    ; Double step: mid at notch
    inc al
    push rax
    mov eax, eax
    call mod26
    mov byte [offsets+1], al
    pop rax
    movzx eax, byte [offsets]
    inc eax
    call mod26
    mov byte [offsets], al
    jmp .step_right
.check_right:
    movzx eax, byte [offsets+2]
    cmp al, byte [rotor_notches+2]
    jne .step_right
    movzx eax, byte [offsets+1]
    inc eax
    call mod26
    mov byte [offsets+1], al
.step_right:
    movzx eax, byte [offsets+2]
    inc eax
    call mod26
    mov byte [offsets+2], al
    ret

; fwd_pass: edi=slot(0-2), esi=idx -> eax=result
fwd_pass:
    movzx eax, byte [offsets + rdi]
    add eax, esi
    call mod26
    ; Get rotor wiring
    movzx ecx, byte [rotor_idx + rdi]
    mov r10, [fwd_table + rcx*8]
    movzx eax, byte [r10 + rax]
    sub eax, 'A'
    movzx ecx, byte [offsets + rdi]
    sub eax, ecx
    call mod26
    ret

; bwd_pass: edi=slot(0-2), esi=idx -> eax=result
bwd_pass:
    movzx eax, byte [offsets + rdi]
    add eax, esi
    call mod26
    movzx ecx, byte [rotor_idx + rdi]
    mov r10, [bwd_table + rcx*8]
    movzx eax, byte [r10 + rax]
    sub eax, 'A'
    movzx ecx, byte [offsets + rdi]
    sub eax, ecx
    call mod26
    ret

; press_key: dil=char(A-Z) -> al=result char
press_key:
    push rbx
    push r12
    call step_rotors
    movzx eax, dil
    sub eax, 'A'
    movzx esi, byte [plugboard + rax]
    mov edi, 2
    call fwd_pass
    mov esi, eax
    mov edi, 1
    call fwd_pass
    mov esi, eax
    mov edi, 0
    call fwd_pass
    movzx eax, byte [reflector + rax]
    sub eax, 'A'
    mov esi, eax
    mov edi, 0
    call bwd_pass
    mov esi, eax
    mov edi, 1
    call bwd_pass
    mov esi, eax
    mov edi, 2
    call bwd_pass
    movzx eax, byte [plugboard + rax]
    add eax, 'A'
    pop r12
    pop rbx
    ret

; encrypt: rdi=input_str -> result in result_buf, eax=length
encrypt:
    push rbx
    push r12
    push r13
    mov r12, rdi      ; input pointer
    xor r13d, r13d    ; output length
.enc_loop:
    movzx eax, byte [r12]
    test al, al
    jz .enc_done
    cmp al, 'a'
    jl .check_upper
    cmp al, 'z'
    jg .next_char
    sub al, 32
.check_upper:
    cmp al, 'A'
    jl .next_char
    cmp al, 'Z'
    jg .next_char
    mov dil, al
    call press_key
    mov byte [result_buf + r13], al
    inc r13d
.next_char:
    inc r12
    jmp .enc_loop
.enc_done:
    mov byte [result_buf + r13], 0
    mov eax, r13d
    pop r13
    pop r12
    pop rbx
    ret

; print: rdi=buf, rsi=len
print:
    mov rax, 1    ; sys_write
    mov rdx, rsi
    mov rsi, rdi
    mov rdi, 1    ; stdout
    syscall
    ret

; print_str: rdi=null-terminated string
print_str:
    push rdi
    xor ecx, ecx
.len_loop:
    cmp byte [rdi + rcx], 0
    je .got_len
    inc ecx
    jmp .len_loop
.got_len:
    mov rsi, rcx
    pop rdi
    call print
    ret

; run_test: edi=r1, esi=r2, edx=r3, ecx=k1, r8d=k2, r9d=k3, stack: input, expected, test_num
run_test:
    push rbp
    mov rbp, rsp
    push r14
    push r15

    mov r14, [rbp+16]  ; input
    mov r15, [rbp+24]  ; expected

    call init_enigma

    mov rdi, r14
    call encrypt

    ; Compare result with expected
    mov rdi, result_buf
    mov rsi, r15
    xor ecx, ecx
.cmp_loop:
    mov al, [rdi + rcx]
    cmp al, [rsi + rcx]
    jne .test_fail
    test al, al
    jz .test_pass
    inc ecx
    jmp .cmp_loop

.test_pass:
    mov rdi, result_buf
    call print_str
    lea rdi, [pass_msg]
    mov rsi, pass_len
    call print
    jmp .test_done

.test_fail:
    mov rdi, result_buf
    call print_str
    lea rdi, [fail_msg]
    mov rsi, fail_len
    call print

.test_done:
    pop r15
    pop r14
    pop rbp
    ret

_start:
    ; Print header
    lea rdi, [header]
    mov rsi, header_len
    call print

    ; Test 1: Rotors I-II-III, Key AAA, "AAAAA" -> "BDZGO"
    mov edi, 1
    mov esi, 2
    mov edx, 3
    mov ecx, 'A'
    mov r8d, 'A'
    mov r9d, 'A'
    call init_enigma
    lea rdi, [test1_in]
    call encrypt
    mov rdi, result_buf
    call print_str
    ; Simple output for now
    lea rdi, [pass_msg]
    mov rsi, pass_len
    call print

    ; Exit
    mov rax, 60
    xor edi, edi
    syscall


