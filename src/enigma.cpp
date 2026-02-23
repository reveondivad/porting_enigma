// Enigma Machine — Rosetta Code Reference (C++)
// Wehrmacht Enigma I: 3 rotors, Reflector B, plugboard.
// PeopleTec Inc. — Guinness World Record Attempt 2026
#include <iostream>
#include <string>
#include <vector>
#include <array>
#include <algorithm>
#include <cctype>
using namespace std;

const string FWD[] = {"EKMFLGDQVZNTOWYHXUSPAIBRCJ","AJDKSIRUXBLHWTMCQGZNPYFVOE","BDFHJLCPRTXVZNYEIWGAKMUSQO"};
const string BWD[] = {"UWYGADFPVZBECKMTHXSLRINQOJ","AJPCZWRLFBDKOTYUQGENHXMIVS","TAGBPCSDQEUFVNZHYIXJWLRKOM"};
const char NOTCH[] = {'Q','E','V'};
const string REFLECTOR = "YRUHQSLDPXNGOKMIEBFZCWVJAT";

class Enigma {
    array<int,3> id{}, off{};
    array<int,26> plug{};
public:
    Enigma(array<int,3> rotors, const string& key, const vector<string>& plugboard={}) : id(rotors) {
        for(int i=0;i<3;i++) off[i]=key[i]-'A';
        for(int i=0;i<26;i++) plug[i]=i;
        for(auto& p:plugboard){ int a=p[0]-'A',b=p[1]-'A'; plug[a]=b; plug[b]=a; }
    }
    int fwd(int r,int i){ int c=(i+off[r])%26; return((FWD[id[r]][c]-'A')-off[r]+26)%26; }
    int bwd(int r,int i){ int c=(i+off[r])%26; return((BWD[id[r]][c]-'A')-off[r]+26)%26; }
    void step(){
        if(off[1]+'A'==NOTCH[id[1]]){ off[1]=(off[1]+1)%26; off[0]=(off[0]+1)%26; }
        else if(off[2]+'A'==NOTCH[id[2]]){ off[1]=(off[1]+1)%26; }
        off[2]=(off[2]+1)%26;
    }
    char pressKey(char ch){
        step(); int i=ch-'A';
        i=plug[i];
        i=fwd(2,i); i=fwd(1,i); i=fwd(0,i);
        i=REFLECTOR[i]-'A';
        i=bwd(0,i); i=bwd(1,i); i=bwd(2,i);
        i=plug[i];
        return 'A'+i;
    }
    string encrypt(const string& text){
        string out;
        for(char c:text){ c=toupper(c); if(c>='A'&&c<='Z') out+=pressKey(c); }
        return out;
    }
};

int main(){
    struct TV { array<int,3> r; string k; vector<string> p; string pt,exp; };
    TV tests[] = {
        {{0,1,2},"AAA",{},"AAAAA","BDZGO"},
        {{0,1,2},"AAA",{},"HELLOWORLD","ILBDAAMTAZ"},
        {{0,1,2},"AAA",{},"ATTACKATDAWN","BZHGNOCRRTCM"},
        {{0,1,2},"MCK",{},"HELLOWORLD","DLTBBQVPQV"},
        {{2,0,1},"AAA",{},"HELLOWORLD","KZHDFQYHXT"},
        {{0,1,2},"AAA",{"AB","CD","EF"},"HELLOWORLD","IKACBBMTBF"},
    };
    bool allOk=true;
    for(int t=0;t<6;t++){
        Enigma e(tests[t].r,tests[t].k,tests[t].p);
        string ct=e.encrypt(tests[t].pt);
        bool ok=ct==tests[t].exp;
        printf("Test %d: %-20s -> %-15s [%s]\n",t+1,tests[t].pt.c_str(),ct.c_str(),ok?"PASS":"FAIL");
        if(!ok) allOk=false;
    }
    cout<<(allOk?"\nALL 6 TESTS PASSED":"\nSOME TESTS FAILED")<<endl;
}
