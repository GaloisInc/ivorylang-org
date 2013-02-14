# Flying Guide

-------------------------------------------------------------------------------

SMACCMPilot is not yet an excellent platform for general purpose flying.
At this time, consider flying to be a demonstration of software functionality,
rather than a demonstration of a fully-featured flight controller.

-------------------------------------------------------------------------------

## Radio Controller Setup

Flight is controlled entirely via the radio controller.

XXX Should put an image here.

Channel 1 controls roll.

Channel 2 controls pitch.

Channel 3 controls throttle.

Channel 4 controls yaw.

Channel 5 selects flight mode. See the flight mode section below.

Channel 6 enables motors to be armed. Motors are armed when all of the
following conditions are met:

1. Channel 6 is high (PWM greater than 1500us) 
2. Throttle is at the low limit (PWM lower than 1050us)
3. Yaw is at the high limit -fully to the right (pwm greater than 1950us)

Motors become disarmed whenever Channel 6 is low (PWM less than 1500us). This
means your channel 6 toggle switch is a "kill switch" which can disable all
motors at any time.

## Flight Modes

Stabilize mode is selected when the three-position switch is in the lowest
position. Always take off in stabilize.

Altitude Hold mode is selected when the three position switch is centered.
Altitude hold will use the PX4FLOW sonar sensor to estimate height off
ground, and attempt to hold the derivative of height off ground at zero
when the throttle stick is near the center. Throttling out of the center 'dead'
zone will control climb rate, and throttling down will control descent rate.
Pushing the throttle stick all the way to the low limit will shut the motors
off.

Loiter mode is selected with the three position switch is in the highest
position. It will use the PX4FLOW optical flow sensor to hold velocity over
ground at zero. Loiter is not fully working at this time.

## Ground Control Station

XXX instructions on how to install and use Mavelous

