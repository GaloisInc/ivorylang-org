# GCS Communication Security

The overall motivation and design of our AES/GCM-inspired communications
security (commsec) design is captured in a Galois [technical note][aes-gcm].
The protocol supports multiple ground stations and a single UAV.  Each entity
(GCS or UAV) has a unique identifier and a unique encryption key which is
specified at build time kin `Keys.mk` (see the [build][build]
for details).

Commsec packets are sent over the air (or alternatively, serial connection)
between SMACCMPilot and the GCS.  Encrypted messages (or a "package") is an
instance of Galois/Counter mode.  A commsec packet contains a header, consisting
of an identifier (4 bytes) and a message counter (4 bytes), the encrypted
message itself, and an authentication tag (8 bytes):

              -------------------------
              | id | cntr | msg | tag |
              -------------------------

In our case, messages are one or more encrypted MAVLink packets.  Messages have
a fixed size of 80 bytes.  Thus, commsec payloads are 80+16 bytes.

Independent of our crypto implementation, we use a simple framing protocol for
marking framing between the radio and the ground station and between radio and
the autopilot, respectively (or if you are testing over serial on the bench,
between the autopilot and GCS).  The purpose of using the framing protocol is to
provide a framing protocol at the link-level independent of MAVLink or a
particular crypto algorithm.

The protocol marks frame boundaries and does no error detection or correction.
Thus, the protocol should only be used in transmissions that can be reasonably
assumed to have zero loss rates.

In the framing protocol, the special byte (0x7e) marks the beginning of the
frame, and a frame includes all bytes until a new begin-frame marker is
encountered.  The first byte of a frame is the frame tag.  Our current tags are
0 to mark air-data messages (messages between the autopilot and the GCS) and 1
to mark radio messages (unencrypted messages to be parsed by the radio).

              ---------------------------------------------
              | 0x7e | tag | frame0 | 0x7e | tag | frame1 |
              ---------------------------------------------

[aes-gcm]: ../artifacts/Galois-commsec.pdf


## Future Work

* New radio firmware supporting the commsec implementation (to be delivered
  soon).

* Key management: we have not implemented any.

* Denial of service: of course, we aren't addressing the problem of radio
  interference.  We have not yet implemented a recovery task/algorithm if
  message fail commsec checks too frequently/for too long.  We plan to use our
  runtime-monitoring framework to do this.  (It is a stretch goal for the demo.)
  The radio firmware/protocol should be considered out-of-scope.

* Epoch numbers.  As a convenience, we intend to implement epoch numbers as a
  convenience.  Epoch numbers stored in non-volatile memory extend the
  "lifespan" of sequence numbers.
