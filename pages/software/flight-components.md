# Flight Software: Components

The SMACCMPilot flight software's origin was a demonstration replacing individual
components of the [Ardupilot flight control software][apm] with components written in
Ivory. It has since evolved to

The system is under active development and the following details are expected to
change. See [future developments][] to get an idea where the flight software is
going.

[apm]: flight-apm.html
[future developments]: flight-future.html

## Components


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

The SMACCMPilot flight controller accepts [MAVLink protocol][mavlink] input.
MAVLink is a common protocol for communication between flight controllers and
ground control stations (GCSs).

Presently, the MAVLink protocol implementation only supports input for setting
telemetry output stream rates.

You can connect to the SMACCMPilot flight controller with mavlink using an [FTDI
cable][ftdi-cable] (or equivalent) on USART1 (broken out to the  6-pin 0.1"
pitch connector on the PX4IOAR) at 57600 baud. We've tested SMACCMPilot's
telemetry interface with [MAVProxy][], a command line MAVLink client.

For more details, see the telemetry input source in
[`SMACCMPilot.Flight.GCS.Receive.Handlers`][rx-handlers]

[rx-handlers]: http://github.com/GaloisInc/smaccmpilot-stm32f4/blob/master/src/flight/SMACCMPilot/Flight/GCS/Receive/Handlers.hs

### Outputs

#### Motor Control

The AR Drone frame's motors are controlled using a single UART stream from the
microcontroller and a set of demultiplexing gates on the PX4IOAR. See the [PX4
Project wiki][px4-ardrone-wiki] and [source code][px4-ardrone-src] for
more details on the PX4IOAR and AR Drone protocol.

[px4-ardrone-src]: http://github.com/PX4/Firmware/blob/master/src/drivers/ardrone_interface/ardrone_motor_control.c 
[px4-ardrone-wiki]: http://pixhawk.ethz.ch/px4/airframes/ar_drone

AR Drone motor control is implemented in the [`ardrone` module][ardrone] of the
`hwf4` library.  We use the [APM Project][apm]'s [`AP_Motors` library][ap-motors]
library for motor mixing. `AP_Motors` is responsible for motor control output
via the [`AP_HAL_SMACCM::RCOutput`][hal-rcout] interface, which is backed by the
`hwf4/ardrone` implementation.

[ardrone]: http://github.com/GaloisInc/smaccmpilot-stm32f4/blob/master/src/bsp/hwf4/include/hwf4/ardrone.h
[ap-motors]: http://github.com/GaloisInc/ardupilot/tree/master/libraries/AP_Motors
[hal-rcout]: http://github.com/GaloisInc/ardupilot/blob/master/libraries/AP_HAL_SMACCM/RCOutput.cpp

Motor control meets the Ivory/Tower implementation of SMACCMPilot in the
[`apmotors_wrapper` interface][motorcontrol-if].

[motorcontrol-if]:http://github.com/GaloisInc/smaccmpilot-stm32f4/blob/master/src/flight/include/flight-support/apmotors_wrapper.h

#### Flight Mode Display

XXX explain more
LED relay

#### Telemetry output

XXX explain more

telemetry output: mavlink streams provided

see [`SMACCMPilot.Flight.GCS.Transmit.Task`][tx-task] for implementation.

[tx-task]: http://github.com/GaloisInc/smaccmpilot-stm32f4/blob/master/src/flight/SMACCMPilot/Flight/GCS/Transmit/Task.hs

[mavlink]: http://qgroundcontrol.org/mavlink/start
[ftdi-cable]: http://www.sparkfun.com/products/9718
[MAVProxy]: http://qgroundcontrol.org/mavlink/mavproxy_startpage

### Sensors

SMACCMPilot uses the gyroscope, accelerometer, and magnetometer on the
[PX4FMU][px4fmu], and the [APM project][apm]'s  [`AP_AHRS`][ap-ahrs] library
for sensor fusion.

XXX explain more: hwf4 i2c and spi, HAL i2c and spi, APM scheduler takes care
of periodic process, sensors ivory/tower interface

[ap-ahrs]: http://github.com/GaloisInc/ardupilot/tree/master/libraries/AP_AHRS
[px4fmu]: ../hardware/flightcontroller.html

