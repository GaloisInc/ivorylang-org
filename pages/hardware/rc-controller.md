# RC Hardware

## What is an RC transmitter?

SMACCMPilot may be flown using a RC transmitter.  When we say "RC" we are
talking about the general range of hobbyist radio control systems built for
flying RC airplanes, helicopters, and the like.

An RC transmitter is a simple and reliable way to manually fly an RC helicopter.
In its current state, the SMACCMPilot flight controller essentially serves to
make a quadcopter system manually controllable - manual inputs command throttle,
pitch angle, roll angle, and yaw rate. We will map these inputs to the four
joystick axes of the RC transmitter. We'll also map the control sequences to arm
and disarm the flight controller to use a switch on the RC transmitter as a
safety.

![*RC Transmitter Joystick Functions*](../images/radio.png)

## Compatible RC Transmitter and Receivers

RC transmitter systems come in many shapes in sizes. Each transmitter system
will also be compatible with some set of RC receivers. We need a transmitter and
receiver system with the following features:

* Aircraft or helicopter style transmitter with two joysticks
* Transmitter offers at least 6 channels of control
* Receiver offers PPM output

### Sample Radio: 9X series w/ FRSKY radio gear

On our [shopping list][] page we recommend the [Turnigy 9XR
radio][9xr-hobbyking]. The 9XR is one of the least expensive radios which can be
configured with all of the features required for SMACCMPilot. The aesthetics and
build quality leave something to be desired, but it gets the job done.

[shopping list]: shoppinglist.html

![*Image: hobbyking.com*](../images/9xr_hobbyking.jpg)

[9xr-hobbyking]: http://hobbyking.com/hobbyking/store/__31544__Turnigy_9XR_Transmitter_Mode_2_No_Module_.html

The 9XR is designed to take interchangeable radio transmitter modules for
inter-operation with various radio receivers. We recommend using [FRSKY][] radio
modules because of low cost and PPM output capability.

The [FRSKY DJT transmitter module][djt] is compatible with the 9XR transmitter.
The [FRSKY D4R-II receiver][d4r-ii] is small, inexpensive, and offers PPM
output.

[FRSKY]: http://www.frsky-rc.com
[djt]: http://www.frsky-rc.com/product/pro.php?pro_id=8
[d4r-ii]: http://www.frsky-rc.com/product/pro.php?pro_id=24

## Mixing information

RC transmitters typically require some amount of setup to configure the mapping
of control inputs to output channels. SMACCMPilot requires a radio with at least
6 output channels. The 9XR recommended above supports 8 channels.

SMACCMPilot expects radio channels according to the following scheme:

* Channel 1 controls roll.
* Channel 2 controls pitch.
* Channel 3 controls throttle.
* Channel 4 controls yaw.
* Channel 5 enables motors to be armed. Motors are armed when all of the
following conditions are met:
    1. Channel 6 is high (PWM greater than 1500us)
    2. Throttle is at the low limit (PWM lower than 1050us)
    3. Yaw is at the high limit -fully to the right (pwm greater than 1950us)
* Channel 6 selects flight control mode. Flight modes are not yet implemented in
  the SMACCMPilot flight control software, but eventually this will allow
  different flight control modes to be toggled from the RC transmitter.

Motors become disarmed whenever Channel 6 is low (PWM less than 1500us). This
means your channel 6 toggle switch is a "kill switch" which can disable all
motors at any time.

### Mixing example for 9x series

In the 9XR radio, setting up the following the mixer screen will give correct
behavior with the 'THR CUT' switch used for arming and the 'AUX.3' switch used
for mode selection.

![](../images/9x-mixerscreen.jpg)


## More information

The [PX4 Project wiki][px4-rc] has more information about various RC radio
systems. Note that the PX4 project software can support more types of radio
system input than the SMACCMPilot software. SMACCMPilot only supports PPM input
at this time.


[px4-rc]: http://pixhawk.ethz.ch/px4/radio-control/start

