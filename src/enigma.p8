-- enigma cipher - pico-8 (fantasy console, lua subset)
-- wehrmacht enigma i, 3 rotors, reflector b, plugboard, double-stepping
-- peopletec inc. - guinness world record attempt 2026

function _init()
 fwd={
  {4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9},
  {0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4},
  {1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14}
 }
 bwd={
  {20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9},
  {0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18},
  {19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12}
 }
 ref={24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19}
 ntch={16,4,21}
 r0=0 r1=1 r2=2
 o0=0 o1=0 o2=0
 n1=ntch[r1+1] n2=ntch[r2+1]
 pb={}
 for i=0,25 do pb[i]=i end
end

function mod26(n)
 return ((n%26)+26)%26
end

function pfwd(rotor,offset,ch)
 local inp=mod26(ch+offset)
 local out=fwd[rotor+1][inp+1]
 return mod26(out-offset)
end

function pbwd(rotor,offset,ch)
 local inp=mod26(ch+offset)
 local out=bwd[rotor+1][inp+1]
 return mod26(out-offset)
end

function step()
 if o1==n1 then
  o1=mod26(o1+1)
  o0=mod26(o0+1)
 elseif o2==n2 then
  o1=mod26(o1+1)
 end
 o2=mod26(o2+1)
end

function presskey(ch)
 step()
 local c=pb[ch]
 c=pfwd(r2,o2,c)
 c=pfwd(r1,o1,c)
 c=pfwd(r0,o0,c)
 c=ref[c+1]
 c=pbwd(r0,o0,c)
 c=pbwd(r1,o1,c)
 c=pbwd(r2,o2,c)
 return pb[c]
end

function encrypt(msg)
 local res=""
 for i=1,#msg do
  local ch=ord(sub(msg,i,i))-65
  res=res..chr(presskey(ch)+65)
 end
 return res
end

function _draw()
 cls(0)
 print("enigma cipher - pico-8",4,4,7)
 _init()
 print("test1: "..encrypt("aaaaa"),4,16,11)
 _init()
 print("test2: "..encrypt("helloworld"),4,24,11)
end
