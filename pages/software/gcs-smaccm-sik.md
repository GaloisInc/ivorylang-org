# SMACCM-SiK 3DR Radio Firmware

SMACCMPilot implements a secure communication link on top of the [3DR Radio][],
an open-source hardware design developed by 3D Robotics for the ArduPilot
project.  3DR Radios provide reliable a low bandwidth (9600baud) radio link over
long ranges (>1mi line of sight). They are available in two frequency bands
(433MHz and 915MHz), making them suitable for unlicensed operation in most
countries.

![3DR Radio](/images/3dr_radio.jpg)

SMACCMPilot uses a custom firmware on the 3DR Radio called [SMACCM-SiK][]. This
firmware is not backwards compatible with existing 3DR Radio firmware. You will
need to flash the SMACCM-SiK firmware to your 3DR Radios after receiving them
from the factory. It is possible to restore your radios to the standard SiK
firmware using the [standard 3DR Radio firmware & configuration
utility][3dr-upload], which runs on Windows.

[smaccm-sik]: https://github.com/GaloisInc/smaccmpilot-SiK
[3DR Radio]: http://store.3drobotics.com/products/3dr-radio-telemetry-kit-915-mhz
[3dr-upload]: http://ardupilot.com/downloads/?did=89

The SMACCM-SiK firmware for the 3DR Radios is a fork of the SiK project,
authored by Michael Smith and Andrew Tridgell. We are grateful to those authors
for their open source contributions, and for assistance with our modifications.

## Installing SMACCM-SiK on your radios

<a class="btn btn-primary"
 href="/artifacts/smaccm-sik-1.6-3drradio.ihx">Download SMACCM-SiK 1.6 Binary</a>

<a class="btn btn-primary"
 href="/artifacts/sik_uploader.py">Download SiK upload script</a>

The latest SMACCM-SiK firmware for the 3DR Radio is provided as a binary for
your convenience. If you would like to build your own from source, see the
development section below.

You should upload the SMACCM-SiK firmware on both radios used for the
SMACCMPilot telemetry link. Using a SMACCM-SiK radio with a standard SiK radio
is not supported and will result in improper operation.

We've also [provided](/artifacts/sik_uploader.py) the firmware upload python
script, also [found in the SiK repository][sik-upload-py], for convenience. To
upload firmware to a 3DR radio, attach it to your computer via a USB to serial
converter, and run:

```
python sik_uploader.py --port ENTER_SERIAL_PORT_HERE smaccm-sik-1.6-3drradio.ihx
```

You will need Python 2.6 and the [pyserial][] package to use the upload script.

[sik-upload-py]: https://github.com/tridge/SiK/blob/master/Firmware/tools/uploader.py
[pyserial]: http://pyserial.sourceforge.net/

## Configuring SMACCM-SiK firmware

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


[SDCC compiler]: http://sdcc.sourceforge.net/
[sik]: https://github.com/tridge/SiK
[sik-readme]: https://github.com/tridge/SiK
[sik-wiki]: http://code.google.com/p/ardupilot-mega/wiki/3DRadio

