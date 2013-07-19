# Flight Software: Future

Informally and roughly, here are our plans for the future:

We'll eventually use ivory-bsp-stm32f4 to replace all hardware support.  This
means eliminating hwf4, the AP\_HAL and other APM project code under the hood.

We may have to make this change in stages since the APM project project provides
some important algorithms. But eventually we'll have to either come up with our
own sensor fusion and motor mixing, or port the AP\_AHRS and AP\_Motor
algorithms to Ivory.

We'll expand the MAVLink implementation:

* to support parameters, which will be integrated with Ivory/Tower. (There is a
  C/Ivory implementation right now but Pat let it bit rot.)
* to support flying via a game pad on your PC so you don't need the RC transmitter anymore.

Additionally, encryption for MAVLink and a better packet mode for 3DR Radios is
in the pipes.

We'll expand the flight code with more features:

* Altitude hold: barometer, sonar
* Position hold (loiter): GPS, PX4FLOW 
* Navigation: we still need to design some sort of waypoint and path planning system

More info to come soon.
