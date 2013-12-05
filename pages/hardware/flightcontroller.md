# Flight Controller

![](/images/fmu-top.jpg)



The [PX4FMU v1.7][fmu] is the heart of the SMACCMPilot platform. It contains
all of the sensors required for basic flight stabilization and the
STM32F4 microcontroller which runs the whole platform. [More info on the PX4
project wiki][fmu].

The [PX4IOAR][] is required as an interface board when using the [AR Drone][]
as an air vehicle. It provides the interface required to talk to the AR Drone
motors, as well as a power regulator and connectors for the PPM radio input and
3DR radio. [More info on the PX4 project wiki][PX4IOAR].

[fmu]: http://pixhawk.ethz.ch/px4/modules/px4fmu
[PX4IOAR]: http://pixhawk.ethz.ch/px4/modules/px4ioar
[AR Drone]: airvehicle-ardrone.html


