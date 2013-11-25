# SMACCM-SiK 3DR Radio Firmware

SMACCMPilot implements a secure communication link on top of the [3DR Radio][],
an open-source hardware design developed by 3D Robotics for the ArduPilot
project.  3DR Radios provide reliable a low bandwidth (9600baud) radio link over
long ranges (>1mi line of sight). They are available in two frequency bands
(433MHz and 915MHz), making them suitable for unlicensed operation in most
countries.

TODO: 3DR Radio image here

SMACCMPilot uses a custom firmware on the 3DR Radio called [SMACCM-SiK][]. This
firmware is not backwards compatible with existing 3DR Radio firmware. You will
need to flash the SMACCM-SiK firmware to your 3DR Radios after receiving them
from the factory. It is possible to restore your radios to the standard SiK
firmware using the [standard 3DR Radio firmware & configuration
utility][3dr-upload].

[smaccm-sik]: https://github.com/GaloisInc/smaccmpilot-SiK
[3DR Radio]: http://store.3drobotics.com/products/3dr-radio-telemetry-kit-915-mhz
[3dr-upload]: http://TODO/FIXME

The SMACCM-SiK firmware for the 3DR Radios is a fork of the SiK project,
authored by Michael Smith and Andrew Tridgell. We are grateful to those authors
for their open source contributions, and for assistance with our modifications.

## Installing SMACCM-SiK on your radios

TODO: package up the upload python script and a binary in a zip file, should be
download, plug, & run.

You may need to change parameters in the radio firmware to select a channel,
frequency range, and other parameters. SMACCM-SiK is compatible with parameters
setup and saved to flash from the standard SiK firmware version 1.6. You can use
the [3DR Radio firmware & configuration utility][3dr-upload] to upload SiK 1.6
and configure radio parameters. You can then flash the radio to use SMACCM-SiK,
and the previous saved parameters will be effective.

You can also update parameters with the SMACCM-SiK firmware loaded with AT
commands from a serial console, however, we do not provide any convenient tools
for changing the parameters on native SMACCM-SiK at this time.

## Differences from SiK

SMACCM-SiK is a fork from the SiK project. We've made the following changes to
the operation of the radios:

* All messages to and from the radios are framed. You can no longer treat paired
  radios as a virtual serial link, but instead must frame over-the-air messages
  into chunks of no more than 96 bytes.
* Messages are sent over the air using the same framing the client provides them
  in. This is designed to simplify stream reconstruction.
* Each message frame has a header byte which specifies whether it is an
  over-the-air message to be sent to the remote radio, or a radio message to
  be interpreted by the local radio's AT command engine. Likewise, each frame
  from the radio has the same header byte marking whether the frame is from the
  remote radio, or a response to a local AT command.
* AT command outputs are bounded, so some long commands will not be fully
  printed in the response.
* New binary mode radio status commands have been implemented in the AT command
  engine. These replace the previous status reporting mechanism of injecting
  MAVLink RADIO packets into the packet stream.
* Remote radio AT commands are disabled, as is remote firmware upload. This
  prevents an untrusted party from disabling some or all transcievers.
* The "+++(wait one second)" AT command mode still exists for backwards
  compatiblity with existing firmware upgrade software. Responses are still
  subject to truncation.
* The existing SiK bootloader is unmodified.

## Developing SMACCM-SiK

SMACCM-SiK is built with the [SDCC compiler][] version 3.1.0. We've had issues
with other versions of this toolchain, so we recommend others build using that
precise version.

SMACCM-SiK uses the same build system as the parent [SiK project][sik]. You can
follow the [README file][sik-readme] and find resources on the [Ardupilot
wiki][sik-wiki].


[SDCC compiler]: http://TODO/FIXME
[sik]: https://github.com/tridge/SiK
[sik-readme]: https://github.com/tridge/SiK/TODO/FIXME
[sik-wiki]: http://code.google.com/p/ardupilot-mega/wiki/3DRadio

