
# Secure MAVLink (SMAVLink)

[MAVLink][mavlink] is the micro air vehicle message format used to communicate
between the ground stations and unmanned vehicles.  MAVLink uses insecure radio
frequency-based communications channels that are susceptible to common attacks
including snooping, forgery, replay attacks, traffic analysis, and denial of
service. In response, we've implemented a light-weight encapsulation format that
can be used with MAVLink to protect against forgery, replay attacks, and
snooping.  The changes result in an overhead of 16 bytes of additional bandwidth
use per MAVLink frame, which is potentially composed of many message.  The
computational overhead is dominated by encryption and decryption operations for
each message send and receive.

SMAVLink is made with the following domain-specific constraints in mind:

* Costs, in terms of computational overhead and bandwidth, must be kept low.
* Availability of the communications medium must not be eliminated by unexpected power-cycles.
* One-way links must remain useful, the protocol can not assume two-way communication.
* Packet-loss can occur for extended periods of time (easily many minutes).

There are two distinct phases for SMAVLink:  [key
agreement](commsec-keyexchange.html) and
[encapsulation](commsec-encapsulation.html). Key agreement is when the various
systems decide on a symmetric key with which they will encrypt and authenticate
the frames. Encapsulation (or decapsulation, when receiving) is the actual
encryption and packaging of data for transmission.

[mavlink]: http://qgroundcontrol.org/mavlink/start
