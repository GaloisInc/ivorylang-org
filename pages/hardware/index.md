# Hardware Overview

## PX4 Autopilot Project

The SMACCMPilot project uses the [PX4 autopilot project][px4] as a hardware
platform. The PX4 Autopilot project provides a number hardware solutions for use by
hobbyists, researchers, and developers.

The PX4 Autopilot Project is an open-source, open-hardware project led by
the [PIXHAWK group][pixhawk] at [ETH Zurich (Swiss Federal Institute
of Technology)][ethz], and supported by [3D Robotics][3dr], a leading
manufacturer of open-source unmanned aerial vehicle technology.

PX4 project hardware is manufactured by [3D Robotics][3dr] and sold
internationally through a [network of distributors][3drdisty].

[px4]: http://pixhawk.ethz.ch/px4
[pixhawk]: http://pixhawk.ethz.ch
[ethz]: http://www.ethz.ch
[3dr]: http://3drobotics.com
[3drdisty]: http://diydrones.com/profiles/blogs/list-of-all-diy-drones

## Flight Controller

![PX4 Autopilot on AR Drone][autopilot_img]

The SMACCMPilot project supports the [PX4FMU 1.7][px4fmu] autopilot board
with the the [PX4IOAR][px4ioar] expansion board. See the [Flight Controller][fc]
page for details on SMACCMPilot support for the various features available
on these boards.

[autopilot_img]: ../images/ardrone_px4_600.jpg

[px4fmu]: http://pixhawk.ethz.ch/px4/modules/px4fmu
[px4ioar]: http://pixhawk.ethz.ch/px4/modules/px4ioar
[fc]: flightcontroller.html


## AR Drone

The SMACCMPilot project uses the [Parrot AR Drone][ardrone] quadcopter as the
basis for an air vehicle. The frame, motors, motor controllers, propellers, and
optional foam fuselage and guard are used by SMACCMPilot, but the included
AR Drone flight controller electronics, cameras, and sensors are removed.

*AR Drone is a trademark of Parrot SA, Paris. The SMACCMPilot project is
not affiliated with Parrot, and does not use any Parrot technology or
software.*

![AR Drone Flight Platform with PX4 electronics installed][modifiedardrone_img]

You will need to disassemble your AR Drone and reassemble it with the
PX4FMU based flight electronics. We recommend following this [AR Drone
repair video][disassembly_video] to disassemble your AR Drone, and the
[PX4 Wiki guide][px4wiki_ardroneassembly] for installing the PX4 electronics.

Note that some AR Drones are sold with an older version of firmware on their
motor controllers. Before disassembling your AR Drone to replace the
electronics, you should connect it to an Android or iOS controller and update
the firmware on the AR Drone, which should update the firmware on the motor
controllers as well.

[disassembly_video]: http://www.youtube.com/watch?v=nESilOcY3tc

[modifiedardrone_img]: ../images/px4ioar.jpg "AR Drone with PX4
electronics installed"
[ardrone]: http://ardrone2.parrot.com
[px4wiki_ardroneassembly]: http://pixhawk.ethz.ch/px4/airframes/ar_drone#assembly

## Accessories

For developing and flying with SMACCMPilot, you will need accessories including
[radio modems][3drradio], a [hobby radio controller][rc], spare batteries,
battery charger, and some miscellaneous cables. We have compiled a [shopping
list][shoppinglist] of all of the parts you will need to build a complete
SMACCMPilot flight platform.

You will also want to purchase and use a [debugger](blackmagic.html) for
SMACCMPilot development.

[3drradio]: http://store.3drobotics.com/products/3dr-radio-telemetry-kit-915-mhz 
[rc]: http://www.turnigy9xr.com
[shoppinglist]: shoppinglist.html

