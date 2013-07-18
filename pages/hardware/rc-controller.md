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

XXX put a diagram here for throttle roll pitch yaw to the sticks

## Compatible RC Transmitter and Receivers

RC transmitter systems come in many shapes in sizes. Each transmitter system
will also be compatible with some set of RC receivers. We need a transmitter and
receiver system with the following features:

* Aircraft or helicopter style transmitter with two joysticks
* Transmitter offers at least 6 channels of control
* Receiver offers PPM output

### Sample Radio: 9X series w/ FRSKY radio gear

On our [shopping list][] page we recommend the Turnigy 9XR radio.

- tell about 9xr, regular 9x
- er9x firmware is recommended

- frsky radio gear has proper receivers in a small size
- frsky transmitter modules plug into 9x (JR sized pocket)


## Mixing information

* Channel 1 controls roll.
* Channel 2 controls pitch.
* Channel 3 controls throttle.
* Channel 4 controls yaw.
* Channel 5 selects flight control mode. Flight modes are not yet implemented in
  the SMACCMPilot flight control software, but eventaully this will allow
  diffent flight control modes to be toggled from the RC transmitter.
* Channel 6 enables motors to be armed. Motors are armed when all of the
following conditions are met:
    1. Channel 6 is high (PWM greater than 1500us) 
    2. Throttle is at the low limit (PWM lower than 1050us)
    3. Yaw is at the high limit -fully to the right (pwm greater than 1950us)

Motors become disarmed whenever Channel 6 is low (PWM less than 1500us). This
means your channel 6 toggle switch is a "kill switch" which can disable all
motors at any time.

### Mixing example for 9x series

## More information

PX4 wiki has more info on this sort of thing


