
# Supported Air Vehicles

SMACCMPilot supports a number of quadcopter air vehicles. Note that at this
time, due to restrictions in the [PX4FMU flight controller][px4fmu], we only
support platforms with 4 motors, but in the future we look forwards to expanding
the range of supported vehicles.

[px4fmu]: flightcontroller.html

## Generic Quadcopter

![3DR Quadcopter](/images/3drquad.jpg)

SMACCMPilot supports any quadcopter platform that uses standard PWM-based
motor controllers. There are many commercial and do-it-yourself options for
the basic flight platform. We use a [3D Robotics ArduCopter Quad][3drquad],
which you can purchase as a kit with motors and motor controllers.

More details on the [generic quadcopter vehicle page][quad-details].

[quad-details]: airvehicle-genericquad.html
[3drquad]: http://store.3drobotics.com/products/3dr-quad-frame-kit-electronics

## AR Drone

![PX4 Autopilot on AR Drone][ardrone_px4]

SMACCMPilot supports the [Parrot AR Drone][ardrone] quadcopter as the basis for
an air vehicle. The frame, motors, motor controllers, propellers, and optional
foam fuselage are used, while the AR Drone flight electronics, cameras, and
sensors are swapped out for the [PX4FMU][] flight controller.

More details on the [AR Drone vehicle page][ardrone-details].

[ardrone-details]: airvehicle-ardrone.md
[ardrone_px4]: /images/ardrone_px4_600.jpg

