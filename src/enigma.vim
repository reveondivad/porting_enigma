" Enigma Cipher - Vim script (Vimscript)
" Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
" PeopleTec Inc. - Guinness World Record Attempt 2026
" Usage: vim -S enigma.vim

let s:fwdI   = [4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9]
let s:fwdII  = [0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4]
let s:fwdIII = [1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14]
let s:bwdI   = [20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9]
let s:bwdII  = [0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18]
let s:bwdIII = [19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12]
let s:ref    = [24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19]
let s:notch  = [16, 4, 21]

function! s:Mod26(n) abort
  let m = a:n % 26
  return m < 0 ? m + 26 : m
endfunction

function! s:GetFwd(r, i) abort
  if a:r == 0 | return s:fwdI[a:i] | endif
  if a:r == 1 | return s:fwdII[a:i] | endif
  return s:fwdIII[a:i]
endfunction

function! s:GetBwd(r, i) abort
  if a:r == 0 | return s:bwdI[a:i] | endif
  if a:r == 1 | return s:bwdII[a:i] | endif
  return s:bwdIII[a:i]
endfunction

function! s:PassFwd(rotor, offset, ch) abort
  let inp = s:Mod26(a:ch + a:offset)
  let out = s:GetFwd(a:rotor, inp)
  return s:Mod26(out - a:offset)
endfunction

function! s:PassBwd(rotor, offset, ch) abort
  let inp = s:Mod26(a:ch + a:offset)
  let out = s:GetBwd(a:rotor, inp)
  return s:Mod26(out - a:offset)
endfunction

let s:r = [0, 0, 0]
let s:o = [0, 0, 0]
let s:n = [0, 0, 0]
let s:pb = range(26)

function! s:Init(r1, r2, r3, k1, k2, k3) abort
  let s:r = [a:r1-1, a:r2-1, a:r3-1]
  let s:o = [a:k1, a:k2, a:k3]
  let s:n = [s:notch[s:r[0]], s:notch[s:r[1]], s:notch[s:r[2]]]
  let s:pb = range(26)
endfunction

function! s:Step() abort
  if s:o[1] == s:n[1]
    let s:o[1] = s:Mod26(s:o[1] + 1)
    let s:o[0] = s:Mod26(s:o[0] + 1)
  elseif s:o[2] == s:n[2]
    let s:o[1] = s:Mod26(s:o[1] + 1)
  endif
  let s:o[2] = s:Mod26(s:o[2] + 1)
endfunction

function! s:PressKey(ch) abort
  call s:Step()
  let c = s:pb[a:ch]
  let c = s:PassFwd(s:r[2], s:o[2], c)
  let c = s:PassFwd(s:r[1], s:o[1], c)
  let c = s:PassFwd(s:r[0], s:o[0], c)
  let c = s:ref[c]
  let c = s:PassBwd(s:r[0], s:o[0], c)
  let c = s:PassBwd(s:r[1], s:o[1], c)
  let c = s:PassBwd(s:r[2], s:o[2], c)
  return s:pb[c]
endfunction

function! s:Encrypt(text) abort
  let result = ''
  for i in range(len(a:text))
    let ch = char2nr(a:text[i]) - 65
    if ch >= 0 && ch < 26
      let result .= nr2char(s:PressKey(ch) + 65)
    endif
  endfor
  return result
endfunction

" Run tests
call s:Init(1,2,3, 0,0,0)
echom 'Enigma Cipher - Vim script'
echom 'Test 1: ' . s:Encrypt('AAAAA') . ' (expected BDZGO)'
call s:Init(1,2,3, 0,0,0)
echom 'Test 2: ' . s:Encrypt('HELLOWORLD') . ' (expected ILBDAAMTAZ)'
call s:Init(1,2,3, 0,0,0)
echom 'Test 3: ' . s:Encrypt('ATTACKATDAWN') . ' (expected BZHGNOCRRTCM)'
call s:Init(1,2,3, 12,2,10)
echom 'Test 4: ' . s:Encrypt('HELLOWORLD') . ' (expected DLTBBQVPQV)'
call s:Init(3,1,2, 0,0,0)
echom 'Test 5: ' . s:Encrypt('HELLOWORLD') . ' (expected KZHDFQYHXT)'
