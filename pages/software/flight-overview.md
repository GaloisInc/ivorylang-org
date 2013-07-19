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

* describe what stabilization control means
* describe where to find the implementation

### Inputs


#### Radio Control input
The primary way to control the SMACCMPilot flight controller is using a [radio
control (RC) transmitter][hardware-rc].

[hardware-rc]: ../hardware/rc-controller.html

When the controller is armed. The four joystick channels on the RC transmitter
directly control the SMACCMPilot stabilization controller. When disarmed, the
joysticks have no effect, the stabilization controller set point is all zero,
and all motor outputs are disabled.

For a complete description of RC transmitter setup, arming, and disarming,
see the [RC transmitter page][hardware-rc].

The signal from the RC transmitter is sent over the air to an RC receiver, which
communicates with SMACCMPilot using a [PPM signal][ppm-signal]. The flight
controller decodes the PPM signal in the [`hwf4` library timer
driver][hwf4-timer]. The decoded signal is accessed by the APM AP\_HAL library
and finally bridged into the SMACCMPilot Ivory/Tower application when the [user
input capture driver][userinput-c] is accessed in
[`SMACCMPilot.Flight.UserInput.Task`][userinput-ivory].


[ppm-signal]: http://skymixer.net/electronics/84-rc-receivers/78-rc-ppm-signal
[hwf4-timer]: http://github.com/GaloisInc/smaccmpilot-stm32f4/blob/master/src/bsp/hwf4/include/hwf4/timer.h
[userinput-c]: http://github.com/GaloisInc/smaccmpilot-stm32f4/blob/master/src/flight/include/flight-support/userinput_capture.h
[userinput-ivory]: http://github.com/GaloisInc/smaccmpilot-stm32f4/blob/master/src/flight/SMACCMPilot/Flight/UserInput/Task.hs

#### Telemetry input

telemetry input: mavlink stream control

### Outputs

#### Motor Control

AR drone motor controllers

#### Flight Mode Display

LED relay

#### Telemetry output

telemetry output: mavlink streams provided

