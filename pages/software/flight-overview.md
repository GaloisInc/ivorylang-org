# Flight Software: Overview

The SMACCMPilot project has produced [quadcopter][] flight control software as a
real-world validation of the project's new software techniques and [languages][].

[quadcopter]: http://en.wikipedia.org/wiki/Quadcopter
[languages]: ../languages/

At this time, the SMACCMPilot flight controller implements a simple
stabilization controller for the [AR Drone quadcopter][hardware].

[hardware]: ../hardware/

### Capabilities

SMACCMPilot implements a simple quadcopter stabilization controller.

* describe what stabilization control means
* describe where to find the implementation

### Inputs

The primary way to control the SMACCMPilot flight controller is using a [radio
control transmitter][hardware-rc].

[hardware-rc]: ../hardware/rc-controller.html

PPM from receiver

- arming, disarming
- channels
- link to detailed radio setup

telemetry input: mavlink stream control

### Outputs

AR drone motor controllers

LED relay

telemetry output: mavlink streams provided

