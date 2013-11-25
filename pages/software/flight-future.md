# Flight Software: Future

Informally and roughly, here are our plans for the future:

We will eventually use ivory-bsp-stm32f4, our pure Ivory driver suite, to
replace all hardware support. That work will allow us to eliminate the untrusted
hwf4, the AP\_HAL and APM provided gyro, accel, magnetometer, and barometer
drivers.

We will also reimplement both the APM project's calibration, sensor fusion and
inertial navigation algorithms in Ivory.

We'll expand the flight code with more features for autonomous flight:

* Position hold (loiter) using GPS & PX4FLOW sensors.
* Navigation: design and implement a waypoint and path planning system

We will expand the capabilities of Tower to support more operating systems,
including POSIX threading and messaging primitives.

We will improve open-source tools for model checking Ivory programs, and expand
the SMACCMPilot test suite.

We will add a networking layer to Tower, which will generate appropriate code to
distribute tasks between different processors & systems, and manage
inter-processor communication on the CAN bus.

