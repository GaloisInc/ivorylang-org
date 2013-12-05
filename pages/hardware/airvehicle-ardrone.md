
# Air Vehicles: AR Drone

![PX4 Autopilot on AR Drone][ardrone_px4]

[ardrone_px4]: ../images/ardrone_px4_600.jpg

The SMACCMPilot project supports the [Parrot AR Drone][ardrone] quadcopter as
the basis for an air vehicle. The frame, motors, motor controllers, propellers,
and optional foam fuselage and guard are used by SMACCMPilot, but the included
AR Drone flight controller electronics, cameras, and sensors are removed.

*AR Drone is a trademark of Parrot SA, Paris. The SMACCMPilot project is
not affiliated with Parrot, and does not use any Parrot technology or
software.*

The PX4FMU 1.7 flight controller interfaces with the AR Drone via the the the
[PX4IOAR][px4ioar] expansion board.

![AR Drone Flight Platform with PX4 electronics installed][modifiedardrone_img]

You will need to disassemble your AR Drone and reassemble it with the
PX4FMU based flight electronics. We recommend following this [AR Drone
repair video][disassembly_video] to disassemble your AR Drone, and the
[PX4 Wiki guide][px4wiki_ardroneassembly] for installing the PX4 electronics.

Note that some AR Drones are sold with an older version of firmware on their
motor controllers. Before disassembling your AR Drone to replace the
electronics, you should connect it to an Android or iOS controller and update
the firmware on the AR Drone, which should update the firmware on the motor
controllers as well.

[disassembly_video]: http://www.youtube.com/watch?v=nESilOcY3tc

[modifiedardrone_img]: ../images/px4ioar.jpg "AR Drone with PX4
electronics installed"
[ardrone]: http://ardrone2.parrot.com
[px4wiki_ardroneassembly]: http://pixhawk.ethz.ch/px4/airframes/ar_drone#assembly

### Accessories

You'll need lithium polymer batteries and a battery charger for the AR Drone.
Although the AR Drone comes with a battery and charger from the factory, we have
found that the following batteries were less expensive (at 10 minutes of flight
time per charge, you'll likely want spares) and could be charged rapidly with a
higher current charger.

* [High current Lithium Polymer battery charger](http://www3.towerhobbies.com/cgi-bin/wti0001p?&I=LXAZZS&P=ML)
* [3 cell 1300mAh Lithium Polymer battery](http://www3.towerhobbies.com/cgi-bin/wti0001p?&I=LXXMP1&P=ML)

*Real talk: lithium polymer batteries are dangerous.* Please be careful when
charging and using lithium polymer batteries. Overcharging, overheating,
impacts, punctures, or shorts can cause the batteries to [ignite][]. We recommend
charging batteries in a [fire retardant bag][firebag], and [safely disposing
of damaged batteries][dispose].

[ignite]: http://www.youtube.com/watch?v=EseOhC8n7ro
[firebag]: http://www3.towerhobbies.com/cgi-bin/wti0001p?&I=LXZKA2&P=7
[dispose]: https://sites.google.com/site/tjinguytech/charging-how-tos/lipo-disposal 

