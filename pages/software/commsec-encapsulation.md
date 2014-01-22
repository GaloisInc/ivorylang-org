
# SMAVLink Commsec: Encapsulation of MAVLink Frames

In order to encrypt and authenticate (or decrypt and verify) messages, the
commsec routines require a 128 bit key, 32 bit salt, counter values, and source
address for the local system.  The key and salt can be distributed in a manner
specified by the [key agreement](commsec-keyexchange.html) phase while the counters
can initially be zero.

## Design Concepts

AES GCM is a NIST-approved, but independently developed, authenticating
encryption algorithm.  Given a unique (96 bit) initialization vector (IV) and secret
AES key, AES GCM provides confidentiallity and integrity.  By careful
construction of IVs commsec also provides replay protection, communications
security that can withstand power-cycles, flexible in key-distribution, and
reduced overhead (when compared with most other transportation security
protocols).

As stated, most the properties are due to the data used to construct IVs and
its interpretation.  The IV is a concatenation of the bits from the salt,
message counter, epoch counter, and system identifier (ID).  These values fill the
following purposes:

* The message counter serves both to ensure the IV is unique and as a sequence
  number with which the receiver can eliminate duplicate or old messages.  The
  sender increments this counter for each packet sent.
* The Epoch counter is incremented by the sender at each of
  the sender's power-cycles. Upon power-cycle the message counter typically
  resets to zero (due to being stored in volitile memory).  Storing the epoch
  counter in non-volitile memory means the IVs are guarenteed to be unique
  across power failures.
* The system identifier serves two purposes.  First, some users desire a
  relaxed security posture in which some senders share the same key - for
  these users the system ID guarentees the IVs from different senders are
  unique.  For multi-key configuratios the system identifier can be used
  as an database index, allowing lookup of keys and salts to decrypt each
  incoming frame (much like an IPSec _Security Parameter Index_).

## Operation Details

When powered-on, all existing keys, salts, epoch-counters, and 16 bit system
identifiers are loaded from non-volitile memory.  These values are stored in RAM
using a dictionary that is indexed by the 16 bit system identifier.  The
epoch-counter associated with the current system's ID is incremented to
avoid re-using IVs in the event of power failure.

In order to send data, a fresh IV is constructed from the 32 bit salt, 32 bit
message counter, 16 bit epoch counter, and 16 bit source identity.  AES Galois
Counter Mode (GCM) is used to encrypt and authenticate the frame.  The message
counter is incremented and stored in RAM.

After encryption, SMAVLink encapsulates the message with an initialization
vector and authentication tag.  The initialization vector is composed of fields
of source identifier, _epoch counter_, and counter.

![Packet format for commsec packets](/images/commsec-packet.png)

On the receiving end, the sender's ID is used to lookup the key, salt, latest message
and epoch counters.  The IV is reconstructed from the transmitted
portions (the counter, epoch counter, and source identifier) and the salt from
the local database.  If the message counter loaded from memory is greater than or equal to
the received message counter, or the epoch counter is greater than its received
counterpart, then the message is discarded. The IV and key is then used to
decrypt and authenticate the message following the AES GCM specification.

Finally, if a message bearing a greater epoch counter is received, and is
successfully authenticated, then this larger epoch counter should be stored in
non-volitile memory. In order to ensure secure communications can survive an
unexpected power failure the most recent key, salt and an epoch counter should
always be stored in non-volitile memory.  The message counter is only stored in
RAM, allowing fast operation and avoiding unnecessary wear on the flash storage.

### Recap: Data Elements

| Value         | Storage       | Size (bits)             | Comments
| -----         | -------       | ----------------        | --------
| Key           | persistent    | 128                     | Should be random
| Salt          | persistent    | 32                      | Should be random
| Counter       | volitile      | 32                      | Starts at 0, resets to 0 at power-cycle
| Epoch Counter | persistent    | 16                      | Starts at 0, incremented at power-cycle (epoch for local system's) or read from flash (epochs for remote systems)
| Identity      | persistent    | 16                      | Set by admin to a unique value for each system

A note on GCM: It is an implementation detail, but important from a hardware
requirement standpoint, that there is additional data computed and stored in RAM
as an optimization (a time-memory trade-off).  This data balloons the total
memory use to ~5KB per key.
