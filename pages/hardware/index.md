# Hardware Overview

This page will describe the basic components needed to build a hardware platform
for SMACCMPilot.

We have compiled a [shopping list][shoppinglist] of all of the parts you will
need to get a basic system up and running.

[shoppinglist]: shoppinglist.html

## Flight Controller

TODO: image of px4fmu1.7 here.

The SMACCMPilot project supports the [PX4FMU 1.7][px4fmu] autopilot hardware.

[px4fmu]: http://pixhawk.ethz.ch/px4/modules/px4fmu
[px4ioar]: http://pixhawk.ethz.ch/px4/modules/px4ioar
[fc]: flightcontroller.html

The SMACCMPilot project uses hardware from the [PX4 autopilot project][px4] as a
platform. The PX4 Autopilot project provides a number hardware solutions for use
by hobbyists, researchers, and developers.

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

## Flight Platforms

### Quadcopter

TODO image here.

SMACCMPilot supports a wide variety of quadcopters that use simple PWM motor
controllers. See the [flight platforms][] page for more details.

### AR Drone

![PX4 Autopilot on AR Drone][ardrone_px4]

[ardrone_px4]: ../images/ardrone_px4_600.jpg

You can fly SMACCMPilot on a modified AR Drone. See the [flight platforms][]
page for more details.

[flight platforms]: flightplatforms.html

## Required Accessories

### Telemetry Modems

SMACCMPilot needs to communicate with a ground control station (GCS) to operate.

You'll need a pair of [3DR Radio][3drradio] radio modems for bidirectional
communication between the air vehicle and GCS software running on your PC.

See the [3DR Radio setup][3drradio-setup] page for information on how to setup
and configure 3DR Radios for use with SMACCMPilot.

[3drradio]: http://store.3drobotics.com/products/3dr-radio-telemetry-kit-915-mhz 
[3drradio-setup]: TODO

### RC Controller

SMACCMPilot is capable of autonomous and GCS guided flight, but requires a
hobby radio controller for safety. See our [hobby radio controller][rc] page for
information on what kind of system you need, and how to set it up.

[rc]: rc-controller.html

### Batteries & Charger

Each flight platform will require appropriate batteries, and a battery charger.
See the [air vehicle platform page][airvehicle] for more information.

## Optional Accessories

### USB Gamepad

You can fly SMACCMPilot from the GCS using a USB gamepad. See the [GCS Software
page][gcs-sw] for more information.

At this time, we support the Logitech XXX and XXX gamepads.

[gcs-sw]: ../software/gcs.html

### Development: JTAG/SWD Debugger

Developers will want to use a JTAG/SWD debugger for inspecting programs as they
run on the PX4FMU. We recommend the [Black Magic Probe](blackmagic.html), but
various other products will work with the STM32F4 microcontroller as well.


