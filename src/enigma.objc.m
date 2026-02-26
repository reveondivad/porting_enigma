// Enigma Machine - Objective-C Implementation
// Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping)
// PeopleTec Inc. - Guinness World Record Attempt 2026

#import <Foundation/Foundation.h>

static const char *FWD[] = {"EKMFLGDQVZNTOWYHXUSPAIBRCJ",
                             "AJDKSIRUXBLHWTMCQGZNPYFVOE",
                             "BDFHJLCPRTXVZNYEIWGAKMUSQO"};
static const char *BWD[] = {"UWYGADFPVZBECKMTHXSLRINQOJ",
                             "AJPCZWRLFBDKOTYUQGENHXMIVS",
                             "TAGBPCSDQEUFVNZHYIXJWLRKOM"};
static const int NOTCH[] = {16, 4, 21};
static const char *REFL = "YRUHQSLDPXNGOKMIEBFZCWVJAT";

static int mod26(int a) { return ((a % 26) + 26) % 26; }

@interface Rotor : NSObject {
    @public
    const char *fwd, *bwd;
    int notch, offset;
}
- (instancetype)initWithNum:(int)num win:(char)win;
- (int)forwardPass:(int)idx;
- (int)backwardPass:(int)idx;
- (void)step;
- (BOOL)atNotch;
@end

@implementation Rotor
- (instancetype)initWithNum:(int)num win:(char)win {
    self = [super init];
    fwd = FWD[num]; bwd = BWD[num]; notch = NOTCH[num];
    offset = win - 'A';
    return self;
}
- (int)forwardPass:(int)idx {
    int contact = mod26(idx + offset);
    return mod26(fwd[contact] - 'A' - offset);
}
- (int)backwardPass:(int)idx {
    int contact = mod26(idx + offset);
    return mod26(bwd[contact] - 'A' - offset);
}
- (void)step { offset = (offset + 1) % 26; }
- (BOOL)atNotch { return offset == notch; }
@end

@interface Enigma : NSObject {
    Rotor *left, *middle, *right;
    int plug[26];
}
- (instancetype)initWithRotors:(int[3])rotors key:(NSString*)key plugboard:(NSArray*)pb;
- (NSString*)encrypt:(NSString*)text;
@end

@implementation Enigma
- (instancetype)initWithRotors:(int[3])rotors key:(NSString*)key plugboard:(NSArray*)pb {
    self = [super init];
    left   = [[Rotor alloc] initWithNum:rotors[0] win:[key characterAtIndex:0]];
    middle = [[Rotor alloc] initWithNum:rotors[1] win:[key characterAtIndex:1]];
    right  = [[Rotor alloc] initWithNum:rotors[2] win:[key characterAtIndex:2]];
    for (int i = 0; i < 26; i++) plug[i] = i;
    for (NSString *pair in pb) {
        int a = [pair characterAtIndex:0] - 'A';
        int b = [pair characterAtIndex:1] - 'A';
        plug[a] = b; plug[b] = a;
    }
    return self;
}
- (void)stepRotors {
    if ([middle atNotch]) { [middle step]; [left step]; }
    else if ([right atNotch]) { [middle step]; }
    [right step];
}
- (char)pressKey:(char)c {
    [self stepRotors];
    int idx = c - 'A';
    idx = plug[idx];
    idx = [right forwardPass:idx];
    idx = [middle forwardPass:idx];
    idx = [left forwardPass:idx];
    idx = REFL[idx] - 'A';
    idx = [left backwardPass:idx];
    idx = [middle backwardPass:idx];
    idx = [right backwardPass:idx];
    idx = plug[idx];
    return 'A' + idx;
}
- (NSString*)encrypt:(NSString*)text {
    NSString *upper = [text uppercaseString];
    NSMutableString *result = [NSMutableString string];
    for (NSUInteger i = 0; i < [upper length]; i++) {
        unichar c = [upper characterAtIndex:i];
        if (c >= 'A' && c <= 'Z')
            [result appendFormat:@"%c", [self pressKey:(char)c]];
    }
    return result;
}
@end

int main(int argc, const char *argv[]) {
    @autoreleasepool {
        NSLog(@"Enigma Machine - Objective-C Implementation");
        NSLog(@"============================================");

        struct { int r[3]; NSString *key; NSArray *plugs; NSString *plain; NSString *expected; } tests[] = {
            {{0,1,2}, @"AAA", @[],                     @"AAAAA",        @"BDZGO"},
            {{0,1,2}, @"AAA", @[],                     @"HELLOWORLD",   @"ILBDAAMTAZ"},
            {{0,1,2}, @"AAA", @[],                     @"ATTACKATDAWN", @"BZHGNOCRRTCM"},
            {{0,1,2}, @"MCK", @[],                     @"HELLOWORLD",   @"DLTBBQVPQV"},
            {{2,0,1}, @"AAA", @[],                     @"HELLOWORLD",   @"KZHDFQYHXT"},
            {{0,1,2}, @"AAA", @[@"AB",@"CD",@"EF"],   @"HELLOWORLD",   @"IKACBBMTBF"},
        };

        BOOL allPass = YES;
        for (int i = 0; i < 6; i++) {
            Enigma *e = [[Enigma alloc] initWithRotors:tests[i].r key:tests[i].key plugboard:tests[i].plugs];
            NSString *cipher = [e encrypt:tests[i].plain];
            BOOL ok = [cipher isEqualToString:tests[i].expected];
            NSLog(@"  Test %d: %@ -> %@ [%@]", i+1, tests[i].plain, cipher, ok ? @"PASS" : @"FAIL");
            if (!ok) { NSLog(@"          Expected %@", tests[i].expected); allPass = NO; }
        }
        NSLog(@"%@", allPass ? @"  ALL 6 TESTS PASSED" : @"  SOME TESTS FAILED");
    }
    return 0;
}
