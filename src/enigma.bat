@echo off
REM Enigma cipher in Windows Batch
setlocal enabledelayedexpansion

set "RF=4 10 12 5 11 6 3 16 21 25 13 19 14 22 24 7 23 20 18 15 0 8 1 17 2 9"
set "REF=24 17 20 7 16 18 11 3 15 23 13 6 14 10 12 8 4 1 5 25 2 22 21 9 0 19"
set "NOTCH0=16" & set "NOTCH1=4" & set "NOTCH2=21"

set idx=0
for %%n in (%RF%) do (set "RF1_!idx!=%%n" & set /a idx+=1)
set idx=0
for %%n in (%REF%) do (set "REFL_!idx!=%%n" & set /a idx+=1)

set "P0=0" & set "P1=0" & set "P2=0"
set "TEXT=HELLOWORLD"
set "RESULT="
set "ALPHA=ABCDEFGHIJKLMNOPQRSTUVWXYZ"

:encrypt_loop
if "!TEXT!"=="" goto :done
set "CH=!TEXT:~0,1!"
set "TEXT=!TEXT:~1!"

REM Find character index
set /a c=-1
for /L %%i in (0,1,25) do (
  if "!ALPHA:~%%i,1!"=="!CH!" set /a c=%%i
)
if !c! LSS 0 goto :encrypt_loop

REM Step rotors
set /a "mid=0"
if !P1! EQU !NOTCH1! set /a mid=1
if !P2! EQU !NOTCH2! set /a "P2=(P2+1) %% 26"
if !mid! EQU 1 set /a "P1=(P1+1) %% 26"
if !P2! EQU !NOTCH2! set /a "P1=(P1+1) %% 26"
set /a "P2=(P2+1) %% 26"

REM Forward through rotor I
set /a "idx=(c+P0) %% 26"
set /a "c=(!RF1_%idx%!-P0+26) %% 26"

REM Reflector
set /a "c=!REFL_%c%!"

set "RESULT=!RESULT!!ALPHA:~%c%,1!"
goto :encrypt_loop

:done
echo !RESULT!
