# Flight Software: Components

The SMACCMPilot flight software is an evolving research project. This page will
attempt to give a high level overview of the various flight software components,
but you may need to dig down into the source code to understand the details. If
you have any questions, please [send the development mailing list an
email!](/about.html)

The system is under active development and the following details are expected to
change. See [future developments][] to get an idea where the flight software is
going.

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

The SMACCMPilot flight controller accepts [MAVLink protocol][mavlink] input over
a [secure communication link][commsec].
MAVLink is a common protocol for communication between flight controllers and
ground control stations (GCSs).

Presently, the MAVLink protocol implementation only supports:

* Setting telemetry output stream rates
* Writing MAVLink parameters
* Arming and disarming the motors
* User control input for override of RC Transmitter

You can connect to the SMACCMPilot flight controller with mavlink using an [FTDI
cable][ftdi-cable] (or equivalent) on USART1 (broken out to the  6-pin 0.1"
pitch connector on the PX4IOAR) at 57600 baud. We've tested SMACCMPilot's
telemetry interface with [MAVProxy][], a command line MAVLink client.

In flight, you should use the [SMACCM-SiK radio firmware][smaccm-sik] for
the [3DR Radio][].

For more details, see the telemetry MAVLink input handler source code in
[`SMACCMPilot.Flight.GCS.Receive.Handlers`][rx-handlers]

[smaccm-sik]: gcs-smaccm-sik.html
[commsec]: gcs-commsec.html
[3DR Radio]: gcs-commsec.html
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

#### Flight Mode

SMACCMPilot has support for the user to select multiple flight modes from the
radio controller. These flight modes are:

* *Stabilize*: RC transmitter provides manual roll, pitch, yaw, and throttle
  control
* *Altitude Hold*: RC transmitter provides manual roll, pitch, and yaw control.
  Throttle is controlled automatically based on the barometer sensor, using the
  RC transmitter's throttle stick to control climb rate
* *Auto*: Control is the same as Altitude Hold mode, except that the user can
  override the RC transmitter through inputs on a [gamepad][] over the secure
  MAVLink radio link.

[gamepad]: gcs-gamepad.html

In addition to supporting multiple control modes, motor output can be armed or
disarmed from the radio controller. The arming mode is an important safety
feature: When motors are armed, they can spin up at any time due to either pilot
or sensor input. See the [Radio control (RC) transmitter][hardware-rc] page for
a description of arming and disarming the SMACCMPilot controller.

SMACCMPilot displays the current flight mode by blinking the red LED on the
PX4FMU, as well as any optional lights connected to the relay port on the
PX4IOAR board.

TODO: Table mapping mode, armed status to blinking

#### Telemetry output

XXX explain more

telemetry output: mavlink streams provided

see [`SMACCMPilot.Flight.GCS.Transmit.Task`][tx-task] for implementation.

[tx-task]: http://github.com/GaloisInc/smaccmpilot-stm32f4/blob/master/src/flight/SMACCMPilot/Flight/GCS/Transmit/Task.hs

[mavlink]: http://qgroundcontrol.org/mavlink/start
[ftdi-cable]: http://www.sparkfun.com/products/9718
[MAVProxy]: http://qgroundcontrol.org/mavlink/mavproxy_startpage

### Sensors

The SMACCMPilot flight controller uses several components from the [APM
(ArduPilot Mega) software project][ardupilot-project]. We maintain [a fork of
the ArduPilot software][ardupilot-standalone] as part of the SMACCMPilot project.
The fork contains a subset of the code from the the ArduPilot project, and in
some cases has been modified for cleaner interaction with the rest of the
SMACCMPilot software stack.

ArduPilot's hardware and operating system abstraction, called the `AP_HAL`,
permits us to use ArduPilot libraries as part of our platform.
ArduPilot support for the STM32F4 hardware and FreeRTOS operating system is
implemented in the `AP_HAL_SMACCM` library terms of the `hwf4` library from the
smaccmpilot-stm32f4 library.

We use ArduPilot's `AP_AHRS` library to perform sensor fusion of the barometer,
magnetometer, accelerometer, and gyroscope sensors on the PX4FMU flight
controller. The result of this sensor fusion is delivered to the rest of the
flight application through the `apwrapper` library, which wraps the C++
ArduPilot library interfaces into C code which can be imported by Ivory.

[ardupilot-project]: http://ardupilot.com
[ardupilot-standalone]: http://github.com/GaloisInc/smaccmpilot-stm32f4/tree/master/src/standalone_apahrs

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

