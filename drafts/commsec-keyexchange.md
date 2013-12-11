# SMAVLink: Lightweight Key Management

<!---
FIXME SADB comments? -->

### Key Exchange via the Station-to-Station Protocol

The [Station to Station protocol][sts-wikipedia] is a widely studied protocol
that is often realized using RSA signatures.  While our first implementation
will be based on manually distributed RSA keys, this protocol is flexible enough
to allow use of EC keys and certificates if so desired.

First, an initiator sends the responder a MAVLink frame containing `g^x,
sha512(pub_I)` and a sha-512 hash of their public key.  The reponder sends `g^x,
AES_K(RSA_pub_R(g^x,g^y)), sha512(pub_R)`.  The initiator then computes
`K=PRF(g^x g^y)`, decrypts the AES ciphertext, verifies the RSA signature
matches `pub_R`, and verifies the encapsulated values are `g^x` and `g^y`.  The
initiator then sends the final message of `AES_K(RSA_pub_I(g^x,g^y))`, which
is verified by the responder.

### Password-based Key Distribution

Similar to how passwords are used to protect home wireless access points, we
also allow for password-based protection of the UAV links.  Specifically, after
a password is provided each system uses [PBKDF2][pbkdf2-ietf] to derive a 128
bit key and 32 bit salt.  Our inputs to PBKDF2 use sha512-hmac for the PRF,
an iteration count of 5000, and 20 octet output. The PBDKDF2 salt is the first
two characters of the SMAVLink password while the PBKDF2 password is the
remaining bytes from the SMAVLink password.

~~~
data = pbkdf2_sha512_hmac(password, salt, 5000, 20)
~~~

The AES key is the first 16 bytes of `data` while the salt is the last 4
bytes.

Notice that the allowance for password-based keys is motivated by more than
simplicity:

* Some users deploy a system other than the base station and UAV which monitors
  and records all communications.  For regulatory or reliability reasons it is
  critical to allow legitimate, passive, monitoring by a third party.

* Bandwidth is a more expensive resource for certain environments and users,
  leading them to pack messages with different destinations into a single
  frame.  This strategy can only work if either the encryption is at the message
  level, which would multiply the encapsulation overhead, or if all the intended
  destinations head the appropriate key.

Both these issues can be resolved by distributing the same password (the same
key) to all systems.  Repeated initialization vectors (IV) are avoided, and
thus confidentiality is retained, by including the senders' unique system
identifications in IV construction (see
[encapsulation](commsec-encapsulation.html)).

Finally, it is possible but inadvisable to generate unique keys and salts for
each system by using the system ID as the PBKDF2 salt.  The issue is that PBKDF2
is an intentionally expensive algorithm - requiring systems to perform PBKDF2
for each observed system ID would open up an obvious denial of service attack
that is likely an order of magitude easier and more effective than the next-best
attack.

[sts-wikipedia]: http://en.wikipedia.org/wiki/Station-to-Station_protocol#Basic_STS
[pbkdf2-ietf]: http://www.ietf.org/rfc/rfc2898.txt
