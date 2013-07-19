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

#### Flight Stabilization

* describe what stabilization control means
* describe where to find the implementation

#### Onboard Sensors

SMACCMPilot uses the gyroscope, accelerometer, and magnetometer on the
[PX4FMU][px4fmu], and the [APM project][apm]'s  [`AP_AHRS`][ap-ahrs] library
for sensor fusion.

[apm]: flight-apm.html
[px4fmu]: ../hardware/flightcontroller.html
[ap-ahrs]: http://github.com/GaloisInc/ardupilot/tree/master/libraries/AP_AHRS

#### RC Control

SMACCMPilot is flown manually with a radio controller. For more info, see the
[Radio Control hardware page](../hardware/rc-controller.html).

#### Telemetry

SMACCMPilot uses the [MAVLink protocol][mavlink] to communicate with Ground
Communication Stations (GCSs) such as [MAVProxy][]. For more info, see
the [Flight Components page](flight-components.html).

At this time, SMACCMPilot implements a small subset of the MAVLink protocol.
SMACCMPilot does not accept commands from a ground control station, it can only
send flight controller state to the ground.

[MAVProxy]: http://qgroundcontrol.org/mavlink/mavproxy_startpage
[mavlink]: http://qgroundcontrol.org/mavlink/start
