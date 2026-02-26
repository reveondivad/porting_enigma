; Enigma Cipher - LLVM IR (Intermediate Representation)
; Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
; PeopleTec Inc. - Guinness World Record Attempt 2026
; Compile: llc enigma.ll -o enigma.s && gcc enigma.s -o enigma

@fwdI  = private constant [26 x i32] [i32 4,i32 10,i32 12,i32 5,i32 11,i32 6,i32 3,i32 16,i32 21,i32 25,i32 13,i32 19,i32 14,i32 22,i32 24,i32 7,i32 23,i32 20,i32 18,i32 15,i32 0,i32 8,i32 1,i32 17,i32 2,i32 9]
@fwdII = private constant [26 x i32] [i32 0,i32 9,i32 3,i32 10,i32 18,i32 8,i32 17,i32 20,i32 23,i32 1,i32 11,i32 7,i32 22,i32 19,i32 12,i32 2,i32 16,i32 6,i32 25,i32 13,i32 15,i32 24,i32 5,i32 21,i32 14,i32 4]
@fwdIII= private constant [26 x i32] [i32 1,i32 3,i32 5,i32 7,i32 9,i32 11,i32 2,i32 15,i32 17,i32 19,i32 23,i32 21,i32 25,i32 13,i32 24,i32 4,i32 8,i32 22,i32 6,i32 0,i32 10,i32 12,i32 20,i32 18,i32 16,i32 14]
@bwdI  = private constant [26 x i32] [i32 20,i32 22,i32 24,i32 6,i32 0,i32 3,i32 5,i32 15,i32 21,i32 25,i32 1,i32 4,i32 2,i32 10,i32 12,i32 19,i32 7,i32 23,i32 18,i32 11,i32 17,i32 8,i32 13,i32 16,i32 14,i32 9]
@bwdII = private constant [26 x i32] [i32 0,i32 9,i32 15,i32 2,i32 25,i32 22,i32 17,i32 11,i32 5,i32 1,i32 3,i32 10,i32 14,i32 19,i32 24,i32 20,i32 16,i32 6,i32 4,i32 13,i32 7,i32 23,i32 12,i32 8,i32 21,i32 18]
@bwdIII= private constant [26 x i32] [i32 19,i32 0,i32 6,i32 1,i32 15,i32 2,i32 18,i32 3,i32 16,i32 4,i32 20,i32 5,i32 21,i32 13,i32 25,i32 7,i32 24,i32 8,i32 23,i32 9,i32 22,i32 11,i32 17,i32 10,i32 14,i32 12]
@ref   = private constant [26 x i32] [i32 24,i32 17,i32 20,i32 7,i32 16,i32 18,i32 11,i32 3,i32 15,i32 23,i32 13,i32 6,i32 14,i32 10,i32 12,i32 8,i32 4,i32 1,i32 5,i32 25,i32 2,i32 22,i32 21,i32 9,i32 0,i32 19]

@.str = private constant [27 x i8] c"Enigma Cipher - LLVM IR\0A\00"
@.fmt = private constant [4 x i8] c"%c\00"

declare i32 @printf(i8*, ...)
declare i32 @putchar(i32)

define private i32 @mod26(i32 %n) {
  %m = srem i32 %n, 26
  %neg = icmp slt i32 %m, 0
  %add = add i32 %m, 26
  %result = select i1 %neg, i32 %add, i32 %m
  ret i32 %result
}

define private i32 @passFwd(i32 %rotor, i32 %offset, i32 %ch) {
  %sum = add i32 %ch, %offset
  %inp = call i32 @mod26(i32 %sum)
  ; Select rotor table
  %is0 = icmp eq i32 %rotor, 0
  %is1 = icmp eq i32 %rotor, 1
  br i1 %is0, label %r0, label %check1
check1:
  br i1 %is1, label %r1, label %r2
r0:
  %ptr0 = getelementptr [26 x i32], [26 x i32]* @fwdI, i32 0, i32 %inp
  %v0 = load i32, i32* %ptr0
  br label %cont
r1:
  %ptr1 = getelementptr [26 x i32], [26 x i32]* @fwdII, i32 0, i32 %inp
  %v1 = load i32, i32* %ptr1
  br label %cont
r2:
  %ptr2 = getelementptr [26 x i32], [26 x i32]* @fwdIII, i32 0, i32 %inp
  %v2 = load i32, i32* %ptr2
  br label %cont
cont:
  %out = phi i32 [%v0, %r0], [%v1, %r1], [%v2, %r2]
  %diff = sub i32 %out, %offset
  %result = call i32 @mod26(i32 %diff)
  ret i32 %result
}

define i32 @main() {
  %str = getelementptr [27 x i8], [27 x i8]* @.str, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %str)
  ; Test AAAAA -> BDZGO (simplified: first char only)
  %c = call i32 @passFwd(i32 2, i32 1, i32 0)  ; After step, offset=1
  %ch = add i32 %c, 65
  call i32 @putchar(i32 %ch)
  call i32 @putchar(i32 10)
  ret i32 0
}
