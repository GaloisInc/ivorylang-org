
# Flight Platforms

## Generic Quadcopter

SMACCMPilot supports any quadcopter platform that uses standard PWM-based
motor controllers. There are many commercial and do-it-yourself options for
the basic flight platform.

You'll need to build your own wiring harness to connect the PX4FMU flight
controller to the rest of the flight electronics. See [these instructions
on the PX4 project wiki][fmu-breakout] for building a wiring harness for
connecting the quadcopter ESCs, telemetry modem, PPM receiver, and power
to the PX4FMU through the 15-pin expansion connector.

At this time, we only support multicopters with four motors.

[fmu-breakout]: http://pixhawk.ethz.ch/px4/users/servo_breakout

### 3D Robotics ArduCopter Quad

![3DR Quadcopter](/images/3drquad.jpg)

We use a [3D Robotics ArduCopter Quad][3drquad],
which you can purchase as a kit with motors and motor controllers.

[3drquad]: http://store.3drobotics.com/products/3dr-quad-frame-kit-electronics

### Other Quadcopters

The PX4 Project wiki has a [list of user-contributed multicopter aiframes](
https://pixhawk.ethz.ch/px4/airframes/multicopters). SMACCMPilot is compatible
with all of the quadcopters listed through the [PX4FMU 15-pin
connector][fmu-breakout] breakout.

### Accessories

You'll need lithium polymer batteries and a battery charger with any of these
quadcopter kits.


* [An example Lithium Polymer battery
  charger](http://www3.towerhobbies.com/cgi-bin/wti0001p?&I=LXAZZS&P=ML)
* [A small Lithium Polymer battery appropriate for the AR Drone and other small
  quadcopters](http://www3.towerhobbies.com/cgi-bin/wti0001p?&I=LXXMP1&P=ML)
* [A larger Lithium Polymer battery appropriate for the 3DR
  Quad](http://www3.towerhobbies.com/cgi-bin/wti0001p?&I=LXAJGY&P=0)


*Real talk: lithium polymer batteries are dangerous.* Please be careful when
charging and using lithium polymer batteries. Overcharging, overheating,
impacts, punctures, or shorts can cause the batteries to [ignite][]. We recommend
charging batteries in a [fire retardant bag][firebag], and [safely disposing
of damaged batteries][dispose].

[ignite]: http://www.youtube.com/watch?v=EseOhC8n7ro
[firebag]: http://www3.towerhobbies.com/cgi-bin/wti0001p?&I=LXZKA2&P=7
[dispose]: https://sites.google.com/site/tjinguytech/charging-how-tos/lipo-disposal 

