// Enigma Machine — Rosetta Code Reference (C#)
// Wehrmacht Enigma I: 3 rotors, Reflector B, plugboard.
// PeopleTec Inc. — Guinness World Record Attempt 2026
using System;
using System.Linq;
using System.Text;

class Enigma {
    static string[] FWD={"EKMFLGDQVZNTOWYHXUSPAIBRCJ","AJDKSIRUXBLHWTMCQGZNPYFVOE","BDFHJLCPRTXVZNYEIWGAKMUSQO"};
    static string[] BWD={"UWYGADFPVZBECKMTHXSLRINQOJ","AJPCZWRLFBDKOTYUQGENHXMIVS","TAGBPCSDQEUFVNZHYIXJWLRKOM"};
    static char[] NOTCH={'Q','E','V'};
    static string REFLECTOR="YRUHQSLDPXNGOKMIEBFZCWVJAT";

    int[] id, off, plug;

    Enigma(int[] rotors, string key, string[] plugboard=null) {
        id=rotors; off=key.Select(c=>c-'A').ToArray();
        plug=Enumerable.Range(0,26).ToArray();
        if(plugboard!=null) foreach(var p in plugboard) {
            int a=p[0]-'A',b=p[1]-'A'; plug[a]=b; plug[b]=a;
        }
    }

    int Fwd(int r,int i){int c=(i+off[r])%26;return(FWD[id[r]][c]-'A'-off[r]+26)%26;}
    int Bwd(int r,int i){int c=(i+off[r])%26;return(BWD[id[r]][c]-'A'-off[r]+26)%26;}

    void Step(){
        if(off[1]+'A'==NOTCH[id[1]]){off[1]=(off[1]+1)%26;off[0]=(off[0]+1)%26;}
        else if(off[2]+'A'==NOTCH[id[2]]){off[1]=(off[1]+1)%26;}
        off[2]=(off[2]+1)%26;
    }

    char PressKey(char ch){
        Step(); int i=ch-'A';
        i=plug[i];
        i=Fwd(2,i);i=Fwd(1,i);i=Fwd(0,i);
        i=REFLECTOR[i]-'A';
        i=Bwd(0,i);i=Bwd(1,i);i=Bwd(2,i);
        i=plug[i];
        return(char)(i+'A');
    }

    string Encrypt(string text) {
        var sb=new StringBuilder();
        foreach(char c in text.ToUpper()) if(c>='A'&&c<='Z') sb.Append(PressKey(c));
        return sb.ToString();
    }

    static void Main(){
        var tests=new(int[],string,string[],string,string)[]{
            (new[]{0,1,2},"AAA",null,"AAAAA","BDZGO"),
            (new[]{0,1,2},"AAA",null,"HELLOWORLD","ILBDAAMTAZ"),
            (new[]{0,1,2},"AAA",null,"ATTACKATDAWN","BZHGNOCRRTCM"),
            (new[]{0,1,2},"MCK",null,"HELLOWORLD","DLTBBQVPQV"),
            (new[]{2,0,1},"AAA",null,"HELLOWORLD","KZHDFQYHXT"),
            (new[]{0,1,2},"AAA",new[]{"AB","CD","EF"},"HELLOWORLD","IKACBBMTBF"),
        };
        bool allOk=true;
        for(int t=0;t<tests.Length;t++){
            var(r,k,p,pt,exp)=tests[t];
            var e=new Enigma(r,k,p);
            string ct=e.Encrypt(pt);
            bool ok=ct==exp;
            Console.WriteLine($"Test {t+1}: {pt,-20} -> {ct,-15} [{(ok?"PASS":"FAIL")}]");
            if(!ok)allOk=false;
        }
        Console.WriteLine(allOk?"\nALL 6 TESTS PASSED":"\nSOME TESTS FAILED");
    }
}
