# Flight Software: Overview

The SMACCMPilot project has produced [quadcopter][] flight control software as a
real-world validation of the project's new software techniques and
[languages][].

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

SMACCMPilot implements a simple quadcopter stabilizer for manual piloting,
and an altitude hold controller based on the barometer sensor.
Autonomous features, such as position hold, are still in progress. For more info
see the [flight software components page][flight-components].

[flight-components]: flight-components.html

#### RC Control

SMACCMPilot is flown manually with a radio controller. For more info, see the
[Radio Control hardware page][rc].

[rc]: ../hardware/rc-controller.html

#### Telemetry

SMACCMPilot uses the [MAVLink protocol][mavlink] to communicate with Ground
Control Stations (GCSs) such as [MAVProxy][]. The air vehicle implementation of
the MAVLink protocol is described further on the [flight components
page][flight-components]. GCS Software is described further on the [gcs software
page][].

[MAVProxy]: http://qgroundcontrol.org/mavlink/mavproxy_startpage
[mavlink]: http://qgroundcontrol.org/mavlink/start
[gcs software page]: gcs-overview.html
