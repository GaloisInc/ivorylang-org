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
[userinput-c]: http://github.com/GaloisInc/smaccmpilot-stm32f4/blob/master/src/apwrapper/include/apwrapper/userinput_capture.h 
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

AR Drone motor control is [implemented in Ivory][ioar-impl] in the
`ivory-px4-hardware` library.  The AR Drone frame's motors are controlled using
a single UART stream from the microcontroller and a set of demultiplexing gates
on the PX4IOAR. See the [PX4 Project wiki][px4-ardrone-wiki] and [source
code][px4-ardrone-src] for more details on the PX4IOAR and AR Drone protocol.

[ioar-impl]: http://github.com/GaloisInc/smaccmpilot-stm32f4/blob/master/src/ivory-px4-hw/SMACCMPilot/Hardware/PX4IOAR/MotorControl.hs
[px4-ardrone-src]: http://github.com/PX4/Firmware/blob/master/src/drivers/ardrone_interface/ardrone_motor_control.c 
[px4-ardrone-wiki]: http://pixhawk.ethz.ch/px4/airframes/ar_drone

SMACCMPilot also supports PWM motor controllers, which are standard on many
hobby quadcopter platforms including the [3DR ArduCopter][3dr-arducopter]. PWM
motor output is also [implemented in Ivory][pwm-impl], and used in the `flight`
application when building for the `px4fmu17_bare_freertos` target.

[pwm-impl]: http://github.com/GaloisInc/smaccmpilot-stm32f4/blob/master/src/ivory-px4-hw/SMACCMPilot/Hardware/PX4IOAR/MotorControl.hs]
[3dr-arducopter]: http://store.3drobotics.com/products/3dr-arducopter-quad-c-frame-kit-1

#### Flight Mode Display

SMACCMPilot has support for the user to select multiple flight modes from the
radio controller. However, at this time, only a stabilization flight mode is
available in SMACCMPilot, so regardless of which flight mode is selected, only
the stabilizer will be active.

In addition to supporting multiple control modes, motor output can be armed or
disarmed from the radio controller. The arming mode is an important safety
feature: When motors are armed, they can spin up at any time due to either pilot
or sensor input. See the [Radio control (RC) transmitter][hardware-rc] page for
a description of arming and disarming the SMACCMPilot controller.

SMACCMPilot displays the current flight mode by blinking the red LED on the
PX4FMU, as well as any optional lights connected to the relay port on the
PX4IOAR board. When the controller is disarmed (motors safe), the blinking
sequence has a long duty cycle (on for at least 2x the time off). When the
controller is armed, the blinking sequence has a short duty cycle (time off 2x
time on).

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

### Stabilization

SMACCMPilot's controller stabilizes a quadcopter: it uses inertial sensor
feedback to control vehicle attitude, so that with no input the vehicle should
maintain a constant level attitude and a constant heading.

Note that maintaining a constant level attitude is not equivelant to maintaining
a constant position or a constant zero velocity. Misalignments between the
flight controller and frame, disturbances such as wind and prop wash, and sensor
drift will all contribute to vehicle motion even with a perfect stabilization
controller. So, the pilot must control vehicle position or velocity by adjusting
the attitude set point.

The SMACCMPilot controller does not control altitude - the pilot controls the
net throttle directly, and must adjust throttle to maintain or change altitude.

