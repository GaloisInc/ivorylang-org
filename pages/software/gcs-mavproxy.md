# MAVProxy

MAVProxy is a console based MAVLink Ground Control Station (GCS) [authored by
Andrew Tridgell][tridge-mavproxy]. The SMACCMPilot project maintains [a fork of
MAVProxy][smaccm-mavlink] which is distributed as a project submodule.

## Supported Commands

The MAVProxy commands supported by SMACCMPilot include the following, but these
are subject to change:


* `alt`      : Show altitude
* `arm`      : Arm motors, assuming kill switch on the hobby radio controller is
               enabled
* `disarm`   : Disarm motors
* `joystick` : Enable joystick flight
* `mode`     : Set SMACCMPilot flight mode
* `param`    : Manage SMACCMPilot parameters
* `status`   : Show flight data
* `stream`   : Set streaming rate
* `watch`    : Watch a MAVLink pattern

Run `help` in the the MAVProxy window for more information.

With respect to arming, the hobby radio controller is considered a safety backup
controller.  See the instructions on
[arming from the radio controller](../hardware/rc-controller.html#mixing-information)
for more information.

