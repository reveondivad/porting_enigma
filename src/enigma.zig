// Enigma Machine - Zig Implementation
// Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping)
// PeopleTec Inc. - Guinness World Record Attempt 2026

const std = @import("std");
const print = std.debug.print;

const FWD = [3][]const u8{ "EKMFLGDQVZNTOWYHXUSPAIBRCJ", "AJDKSIRUXBLHWTMCQGZNPYFVOE", "BDFHJLCPRTXVZNYEIWGAKMUSQO" };
const BWD = [3][]const u8{ "UWYGADFPVZBECKMTHXSLRINQOJ", "AJPCZWRLFBDKOTYUQGENHXMIVS", "TAGBPCSDQEUFVNZHYIXJWLRKOM" };
const NOTCH = [3]u8{ 16, 4, 21 };
const REFL: []const u8 = "YRUHQSLDPXNGOKMIEBFZCWVJAT";

fn mod26(a: i32) u8 {
    return @intCast(@mod(@mod(a, 26) + 26, 26));
}

const Rotor = struct {
    fwd: []const u8,
    bwd: []const u8,
    notch: u8,
    offset: u8,

    fn init(num: usize, win: u8) Rotor {
        return .{ .fwd = FWD[num], .bwd = BWD[num], .notch = NOTCH[num], .offset = win - 'A' };
    }

    fn forwardPass(self: *const Rotor, idx: u8) u8 {
        const contact = mod26(@as(i32, idx) + @as(i32, self.offset));
        return mod26(@as(i32, self.fwd[contact] - 'A') - @as(i32, self.offset));
    }

    fn backwardPass(self: *const Rotor, idx: u8) u8 {
        const contact = mod26(@as(i32, idx) + @as(i32, self.offset));
        return mod26(@as(i32, self.bwd[contact] - 'A') - @as(i32, self.offset));
    }

    fn step(self: *Rotor) void {
        self.offset = (self.offset + 1) % 26;
    }

    fn atNotch(self: *const Rotor) bool {
        return self.offset == self.notch;
    }
};

const Enigma = struct {
    left: Rotor,
    middle: Rotor,
    right: Rotor,
    plug: [26]u8,

    fn init(rotors: [3]usize, key: []const u8, plugboard: []const [2]u8) Enigma {
        var e: Enigma = undefined;
        e.left = Rotor.init(rotors[0], key[0]);
        e.middle = Rotor.init(rotors[1], key[1]);
        e.right = Rotor.init(rotors[2], key[2]);
        for (0..26) |i| e.plug[i] = @intCast(i);
        for (plugboard) |pair| {
            const a = pair[0] - 'A';
            const b = pair[1] - 'A';
            e.plug[a] = b;
            e.plug[b] = a;
        }
        return e;
    }

    fn stepRotors(self: *Enigma) void {
        if (self.middle.atNotch()) {
            self.middle.step();
            self.left.step();
        } else if (self.right.atNotch()) {
            self.middle.step();
        }
        self.right.step();
    }

    fn pressKey(self: *Enigma, c: u8) u8 {
        self.stepRotors();
        var idx: u8 = c - 'A';
        idx = self.plug[idx];
        idx = self.right.forwardPass(idx);
        idx = self.middle.forwardPass(idx);
        idx = self.left.forwardPass(idx);
        idx = REFL[idx] - 'A';
        idx = self.left.backwardPass(idx);
        idx = self.middle.backwardPass(idx);
        idx = self.right.backwardPass(idx);
        idx = self.plug[idx];
        return idx + 'A';
    }

    fn encrypt(self: *Enigma, text: []const u8, buf: []u8) []u8 {
        var len: usize = 0;
        for (text) |c| {
            var ch = c;
            if (ch >= 'a' and ch <= 'z') ch -= 32;
            if (ch >= 'A' and ch <= 'Z') {
                buf[len] = self.pressKey(ch);
                len += 1;
            }
        }
        return buf[0..len];
    }
};

pub fn main() void {
    print("Enigma Machine - Zig Implementation\n", .{});
    print("====================================\n", .{});

    const Test = struct { rotors: [3]usize, key: []const u8, plugs: []const [2]u8, plain: []const u8, expected: []const u8 };
    const no_plugs: []const [2]u8 = &.{};
    const plugs_abcdef: []const [2]u8 = &.{ .{ 'A', 'B' }, .{ 'C', 'D' }, .{ 'E', 'F' } };

    const tests = [_]Test{
        .{ .rotors = .{ 0, 1, 2 }, .key = "AAA", .plugs = no_plugs, .plain = "AAAAA", .expected = "BDZGO" },
        .{ .rotors = .{ 0, 1, 2 }, .key = "AAA", .plugs = no_plugs, .plain = "HELLOWORLD", .expected = "ILBDAAMTAZ" },
        .{ .rotors = .{ 0, 1, 2 }, .key = "AAA", .plugs = no_plugs, .plain = "ATTACKATDAWN", .expected = "BZHGNOCRRTCM" },
        .{ .rotors = .{ 0, 1, 2 }, .key = "MCK", .plugs = no_plugs, .plain = "HELLOWORLD", .expected = "DLTBBQVPQV" },
        .{ .rotors = .{ 2, 0, 1 }, .key = "AAA", .plugs = no_plugs, .plain = "HELLOWORLD", .expected = "KZHDFQYHXT" },
        .{ .rotors = .{ 0, 1, 2 }, .key = "AAA", .plugs = plugs_abcdef, .plain = "HELLOWORLD", .expected = "IKACBBMTBF" },
    };

    var all_pass = true;
    var buf: [64]u8 = undefined;

    for (tests, 0..) |t, i| {
        var e = Enigma.init(t.rotors, t.key, t.plugs);
        const cipher = e.encrypt(t.plain, &buf);
        const ok = std.mem.eql(u8, cipher, t.expected);
        print("  Test {d}: {s} -> {s} [{s}]\n", .{ i + 1, t.plain, cipher, if (ok) "PASS" else "FAIL" });
        if (!ok) { print("          Expected {s}\n", .{t.expected}); all_pass = false; }
    }
    print("\n  {s}\n", .{if (all_pass) "ALL 6 TESTS PASSED" else "SOME TESTS FAILED"});
}
