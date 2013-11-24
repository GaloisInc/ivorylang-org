
# Flight Platforms

SMACCMPilot supports a number of quadcopter flight platforms. Note that at this
time, due to restrictions in the [PX4FMU flight controller][px4fmu], we only
support platforms with 4 motors, but in the future we look forwards to expanding
the range of supported vehicles.

## Generic Quadcopter

TODO: 3dr quad image here

SMACCMPilot supports any quadcopter platform that uses standard PWM-based
motor controllers. There are many commercial and do-it-yourself options for
the basic flight platform. We use a [3D Robotics ArduCopter Quad][3drquad],
which you can purchase as a kit with motors and motor controllers.

More details on the [Generic Quadcopter Flight Platform][quad-details] page.

[quad-details]: flightplatform-genericquad.html
[3drquad]: http://3drobotics.com/TODO/FIXME

## AR Drone

![PX4 Autopilot on AR Drone][ardrone_px4]

SMACCMPilot supports the [Parrot AR Drone][ardrone] quadcopter as the basis for
an air vehicle. The frame, motors, motor controllers, propellers, and optional
foam fuselage are used, while the AR Drone flight electronics, cameras, and
sensors are swapped out for the [PX4FMU][] flight controller.

More details on the [AR Drone Flight Platform][ardrone-details] page

[ardrone-details]: flightplatform-ardrone.md
[ardrone_px4]: ../images/ardrone_px4_600.jpg

