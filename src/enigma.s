# Enigma cipher in GNU Assembly (AT&T syntax, x86-64)
    .section .data
rotor_fwd_1:
    .byte 4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9
rotor_fwd_2:
    .byte 0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4
rotor_fwd_3:
    .byte 1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14
rotor_bwd_1:
    .byte 20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9
rotor_bwd_2:
    .byte 0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18
rotor_bwd_3:
    .byte 19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12
reflector:
    .byte 24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19
notches:
    .byte 16, 4, 21
positions:
    .byte 0, 0, 0
msg:    .asciz "HELLOWORLD"
fmt:    .asciz "%c"
newline: .asciz "\n"

    .section .text
    .globl main

# mod26: eax = input, returns eax = result
mod26:
    pushq %rbx
    movl $26, %ebx
    cdq
    idivl %ebx          # eax = quotient, edx = remainder
    movl %edx, %eax
    testl %eax, %eax
    jns .mod26_done
    addl $26, %eax
.mod26_done:
    popq %rbx
    ret

# rotor_pass: rdi=wiring_ptr, esi=c, edx=pos -> eax=result
rotor_pass:
    pushq %rbx
    pushq %r12
    movl %edx, %r12d    # save pos
    movl %esi, %eax
    addl %edx, %eax     # c + pos
    call mod26
    movzbl (%rdi,%rax), %eax  # wiring[mod26(c+pos)]
    subl %r12d, %eax    # - pos
    call mod26
    popq %r12
    popq %rbx
    ret

main:
    pushq %rbp
    movq %rsp, %rbp
    # Process each character of msg
    leaq msg(%rip), %rbx
.loop:
    movzbl (%rbx), %eax
    testl %eax, %eax
    jz .done
    subl $65, %eax      # c = ch - 'A'
    cmpl $25, %eax
    ja .next
    # Forward through rotor 3
    leaq rotor_fwd_3(%rip), %rdi
    movl %eax, %esi
    movzbl positions+2(%rip), %edx
    call rotor_pass
    # Reflector
    leaq reflector(%rip), %rdi
    movzbl (%rdi,%rax), %eax
    addl $65, %eax
    movl %eax, %esi
    leaq fmt(%rip), %rdi
    xorl %eax, %eax
    call printf
.next:
    incq %rbx
    jmp .loop
.done:
    leaq newline(%rip), %rdi
    call printf
    xorl %eax, %eax
    popq %rbp
    ret
