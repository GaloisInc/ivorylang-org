# SMAVLink: Lightweight Key Management

<!---
FIXME SADB comments? -->

### Key Exchange via the Station-to-Station Protocol

The [Station to Station protocol][sts-wikipedia] is a widely studied protocol
that is often realized using RSA signatures.  While our first implementation
will be based on manually distributed RSA keys, this protocol is flexible enough
to allow use of EC keys and certificates if so desired.

Let `g` be the public generator value, `x` and `y` be random values modulo a
large prime, `pub_I` be the public key of the initiator, and `pub_R` be the
public key of the responder. `AES_k(m)` is the AES encryption of message `m`
using key `k`.  `RSA_k(m)` is the RSA signature of message `m` using public key
`k`.

First, an initiator generates `x` and sends a MAVLink frame containing `g^x,
sha512(pub_I)`.  The reponder generates `y`, computes `K=PRF(g^x^y)` and sends
`g^y, AES_K(RSA_pub_R(g^x,g^y)), sha512(pub_R)`.  The initiator then computes
`K=PRF(g^y^x)`, decrypts the AES ciphertext, verifies the RSA signature
matches `pub_R`, and verifies the encapsulated values are `g^x` and `g^y`.  The
initiator then sends the final message of `AES_K(RSA_pub_I(g^x,g^y))`, which is
verified by the responder prior to recording the key.

The prime used by MAVLink is borrowed from [RFC5114 Section 2.3][ietf-dh] and reproduced
here:

~~~
mavlink_prime2048 =
0x87A8E61DB4B6663CFFBBD19C651959998CEEF608660DD0F25D2CEED4435E3B00E00DF8F1D61957D4FAF7DF4561B2AA3016C3D91134096FAA3BF4296D830E9A7C209E0C6497517ABD5A8A9D306BCF67ED91F9E6725B4758C022E0B1EF4275BF7B6C5BFC11D45F9088B941F54EB1E59BB8BC39A0BF12307F5C4FDB70C581B23F76B63ACAE1CAA6B7902D52526735488A0EF13C6D9A51BFA4AB3AD8347796524D8EF6A167B5A41825D967E144E5140564251CCACB83E6B486F6B3CA3F7971506026C0B857F689962856DED4010ABD0BE621C3A3960A54E710C375F26375D7014103A4B54330C198AF126116D2276E11715F693877FAD7EF09CADB094AE91E1A1597

mavlink_generator = 5
~~~

### Password-based Key Distribution

Similar to how passwords are used to protect home wireless access points, we
also allow for password-based protection of the UAV links.  Specifically, after
a password is provided each system uses [PBKDF2][pbkdf2-ietf] to derive a 128
bit key and 32 bit salt.  Our inputs to PBKDF2 use sha512-hmac for the PRF,
an iteration count of 5000, and 20 octet output. The PBDKDF2 salt is the 32-byte
SHA2-512 hash of the SMAVLink password while the PBKDF2 password is the
the SMAVLink password.

NOTE: The generation of a salt from a password negates the ability of the salt
to prevent dictioary attacks.  The current method is only proposed because there
is no out-of-band data that can be used as the source of a salt (ex: network
name).  This might change in the future.

~~~
data = pbkdf2_sha512_hmac(password, salt, 5000, 20)
~~~

The AES key is the first 16 bytes of `data` while the salt is the last 4
bytes.

Notice that the allowance for password-based keys is motivated by more than
simplicity:

* Some users deploy a system, in addition to the base station and UAV, which
  monitors and records all communications.  For regulatory or reliability
  reasons it is critical to allow legitimate, passive, monitoring by this
  party.

* Bandwidth is a more expensive resource for certain environments and users,
  leading them to pack messages with different destinations into a single
  frame.  This strategy can only work if either the encryption is at the message
  level, which would multiply the encapsulation overhead, or if all the intended
  destinations hold the appropriate key.

Both these issues can be resolved by distributing the same password (the same
key) to all systems.  Repeated initialization vectors (IV) are avoided, and
thus confidentiality is retained, by including the senders' unique system
identifications in IV construction (see
[encapsulation](commsec-encapsulation.html)).

Finally, it is possible but inadvisable to generate unique keys and salts for
each system by using the system ID as a parameter to PBKDF2. The issue is that
PBKDF2 is an intentionally expensive algorithm - requiring systems to perform
PBKDF2 for each observed system ID would open up an obvious denial of service
attack that is likely an order of magitude easier and more effective than the
next-best attack.

[sts-wikipedia]: http://en.wikipedia.org/wiki/Station-to-Station_protocol#Basic_STS
[pbkdf2-ietf]: http://www.ietf.org/rfc/rfc2898.txt
[ietf-dh]: http://tools.ietf.org/html/rfc5114#section-2.3
