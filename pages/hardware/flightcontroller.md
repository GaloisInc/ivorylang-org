# Flight Controller

The goal of this page is to outline the features of the PX4 hardware that we
care about in the context of SMACCMPilot.

### Hardware Components

The [PX4FMU v1.7][] is the heart of the SMACCMPilot platform. It contains
all of the sensors required for basic flight stabilization and the
STM32F4 microcontroller which runs the whole platform.

The [PX4IOAR][] is also used to interface with the motors, PPM radio
input, 3DR radio, and relay status light.

[PX4FMU v1.7]: http://pixhawk.ethz.ch/px4/modules/px4fmu
[PX4IOAR]:     http://pixhawk.ethz.ch/px4/modules/px4ioar

### TODO: Discuss the following hardware interfaces

* Sensor IO: accel/gyro, compass, barometer
* Motor IO: serial + select lines
* PPM Input
* 3DR Radio
* EEPROM
* Relay Status LED

We'll get back to expand this page at some point soonish. (if not done by july
please yell at pat)
