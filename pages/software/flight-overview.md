# Flight Software: Overview

The SMACCMPilot project has produced [quadcopter][] flight control software as a
real-world validation of the project's new software techniques and [languages][].

[quadcopter]: http://en.wikipedia.org/wiki/Quadcopter
[languages]: ../languages/

### Platforms

The SMACCMPilot flight controller supports several [air vehicles][] and has
operating systems. SMACCMPilot may be built for multiple platforms, which
specify the air vehicle and operating system at compile time. See the [flight
platforms page][] for more info on supported platforms and configuring the
SMACCMPilot build.

[air vehicles]: ../hardware/airvehicle-overview.html
[flight platforms page]: flight-platforms.html

### Capabilities

#### Flight Stabilization

SMACCMPilot implements a simple quadcopter stabilizer for manual piloting.
Autonomous features are still in progress. For more info see the [Flight
Components page][flight-components].

[flight-components]: flight-components.html

#### Onboard Sensors

SMACCMPilot uses the gyroscope, accelerometer, and magnetometer on the
[PX4FMU][px4fmu], and the [APM project][apm]'s  [`AP_AHRS`][ap-ahrs] library
for sensor fusion.

[apm]: flight-apm.html
[px4fmu]: ../hardware/flightcontroller.html
[ap-ahrs]: http://github.com/GaloisInc/ardupilot/tree/master/libraries/AP_AHRS

#### RC Control

SMACCMPilot is flown manually with a radio controller. For more info, see the
[Radio Control hardware page][rc]

[rc]: ../hardware/rc-controller.html

#### Telemetry

SMACCMPilot uses the [MAVLink protocol][mavlink] to communicate with Ground
Communication Stations (GCSs) such as [MAVProxy][]. For more info, see
the [Flight Components page][flight-components]

At this time, SMACCMPilot implements a small subset of the MAVLink protocol.
SMACCMPilot does not accept commands from a ground control station, it can only
send flight controller state to the ground.

[MAVProxy]: http://qgroundcontrol.org/mavlink/mavproxy_startpage
[mavlink]: http://qgroundcontrol.org/mavlink/start
