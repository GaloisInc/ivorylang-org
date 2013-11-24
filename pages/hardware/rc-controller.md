# RC Hardware

## What is an RC transmitter?

SMACCMPilot may be flown using a Radio Control (RC) transmitter.  When we say
"RC" we are talking about the general range of hobbyist radio control systems
built for flying RC airplanes, helicopters, and the like.

An RC transmitter is a simple and reliable way to manually fly a small
quadcopter.  In the stabilize flight mode, the SMACCMPilot flight controller
essentially serves to make a quadcopter system manually controllable - manual
inputs command throttle, pitch angle, roll angle, and yaw rate. We will map
these inputs to the four joystick axes of the RC transmitter. We'll also map the
control sequences to arm and disarm the flight controller to use a switch on the
RC transmitter as a kill switch, which can turn off the quadcopter motors at any
time. This is a critical safety feature.

![*RC Transmitter Joystick Functions*](../images/radio.png)

## A Safety Controller

SMACCMPilot uses the RC system primarily for safety. Because SMACCMPilot is
still experimental and cannot safely fly without human guidance, the RC system
provides a pilot a way to disable autonomous flight or the helicopter motors
themselves at any time.

We envision that one day, SMACCMPilot will be smart enough to fly & land safety
without requiring an RC system for safety. However, for now, we recommend that
SMACCMPilot users get comfortable flying the vehicle under stabilize mode, and
always have a safety pilot holding the RC transmitter when flying in autonomous
mode.

*Real talk: quadcopters are dangerous, and proper use of the RC transmitter will
keep you safe.* Improper operation of any quadcopter can cause injury, and many
of the larger ones can send you to the hospital with cuts or worse. Please take
safe operation seriously, only operate a safe distance away from people, and
always an operator spotting the vehicle and ready to hit the kill switch.

## Compatible RC Transmitter and Receivers

RC transmitter systems come in many shapes in sizes. Each transmitter system
will also be compatible with some set of RC receivers. We need a transmitter and
receiver system with the following features:

* Aircraft or helicopter style transmitter with two joysticks
* Transmitter offers at least 6 channels of control
* Receiver offers PPM output

## Mixing information

RC transmitters typically require some amount of setup to configure the mapping
of control inputs to output channels. SMACCMPilot requires a radio with at least
6 output channels.

RC transmitters use various schemes to "mix" input sticks and switches to the
channel outputs. Fundamentally, each channel is sent from the receiver to the
flight controller as a pulse-width modulated (PWM) wave. (Pulse-position
modulation, PPM, is a scheme used to multiplex multiple PWM signals serially.)
Whether modulated as PWM or PPM, each channel has a width in the time domain,
measured in microseconds. RC transmitters generally output a channel with a
width ranging from 1000us to 2000us. If your RC transmitter uses a different
range, it may not work with SMACCMPilot.

SMACCMPilot expects radio channels according to the following scheme:

* Channel 1 controls roll.
* Channel 2 controls pitch.
* Channel 3 controls throttle.
* Channel 4 controls yaw.

* Channel 5 selects flight control mode. At this time, SMACCMPilot supports
  three flight control modes: stabilize, and altitude hold, and autonomous.
  The user will typically map channel 5 to a 3-position switch on the right side
  of the controller.
  Channel 5 pulse widths correspond to the following modes:
    * 1000-1250us: autonomous mode
    * 1250-1750us: altitude hold mode
    * 1750-2000us: stabilize mode

* Channel 6 is the arming switch. It is designed for safety: if, at any point, the
  arming switch is released, all of the motors will disarm (no more power will
  be applied) instantly. The arming switch must be set before the motors are
  armed via either the RC controller or via a telemetry command.

  The user will typically map channel 6 to a 2-position switch on the top left
  of the controller.

  The user can arm the motors from the RC controller with the following
  sequence:
    1. Channel 6 is set (pulse width greater than 1500us)
    2. Channel 5 set to stabilize mode (pulse width greater than 1750us)
    2. Throttle stick is at the low limit (pulse width lower than 1050us)
    3. Yaw stick is at the high limit - fully to the right (pulse width greater than 1950us)

  After this sequence is complete, the motors are armed, and will begin
  spinning at idle throttle. When the throttle stick is raised, motor power will
  increase, and the vehicle will take off.

## More information

See our [examples page][] for recommendations and setup instructions for some
radio systems we have used.

[examples page]: rc-controller-examples.html

The [PX4 Project wiki][px4-rc] has more information about various RC radio
systems. Note that the PX4 project software can support more types of radio
system input than the SMACCMPilot software. SMACCMPilot only supports PPM input
at this time.

[px4-rc]: http://pixhawk.ethz.ch/px4/radio-control/start
