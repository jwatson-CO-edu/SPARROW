////////// INIT ////////////////////////////////////////////////////////////////////////////////////
const std            = @import("std");
const expect         = std.testing.expect;



////////// RANDOM NUMBERS //////////////////////////////////////////////////////////////////////////

test "random numbers" {
    // Here we create a new RNG using a 64 bit random seed. 
    
    var prng = std.rand.DefaultPrng.init(blk: {
        var seed: u64 = undefined;
        try std.os.getrandom(std.mem.asBytes(&seed));
        break :blk seed;
    });
    const rand = prng.random();

    // `a`, `b`, `c`, and `d` are given random values via this RNG. 
    const a = rand.float(f32);
    const b = rand.boolean();
    // The expressions giving c and d values are equivalent. 
    const c = rand.int(u8);
    const d = rand.intRangeAtMost(u8, 0, 255);
    // Default RNG is Xoroshiro128; there are other prngs available in std.rand.

    //suppress unused local constant compile error
    if (false) _ = .{ a, b, c, d }; 
}



test "##### crypto random numbers #####" {
    // Cryptographically secure random is also available.
    const rand = std.crypto.random;

    const a = rand.float(f32);
    const b = rand.boolean();
    const c = rand.int(u8);
    const d = rand.intRangeAtMost(u8, 0, 255);

    //suppress unused local constant compile error
    if (false) _ = .{ a, b, c, d }; 
}

// Crypto

// std.crypto includes many cryptographic utilities, including:
//     AES (Aes128, Aes256)
//     Diffie-Hellman key exchange (x25519)
//     Elliptic-curve arithmetic (curve25519, edwards25519, ristretto255)
//     Crypto secure hashing (blake2, Blake3, Gimli, Md5, sha1, sha2, sha3)
//     MAC functions (Ghash, Poly1305)
//     Stream ciphers (ChaCha20IETF, ChaCha20With64BitNonce, XChaCha20IETF, Salsa20, XSalsa20)
// This list is inexhaustive. For more in-depth information, try A tour of std.crypto in Zig 0.7.0 - Frank Denis.










////////// MAIN ////////////////////////////////////////////////////////////////////////////////////

pub fn main() void {
    // NOTE: Unused local variables will throw compiler errors!


}