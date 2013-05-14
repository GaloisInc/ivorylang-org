# Shopping List

For your convenience, links to each of the components you need to build a
complete system. Many of these components and vendors have alternatives, this
is not an endorsement of any particular brand or vendor.

-------------------------------------------------------------------------------

## PX4 Autopilot

3D Robotics makes and sells the PX4 Autopilot system. [Other
distributors][3drdisty] carry 3DR products worldwide.

PX4FMU v1.7 [from 3D Robotics][px4fmu_store].

PX4IOAR [from 3D Robotics][px4ioar_store].

PX4FMU and PX4IOAR packaged as a kit [from 3D Robotics][px4kit_store].

[3drdisty]: http://diydrones.com/profiles/blogs/list-of-all-diy-drones

[px4fmu_store]: http://store.3drobotics.com/products/px4fmu-1
[px4ioar_store]: http://store.3drobotics.com/products/px4ioar-1-2
[px4kit_store]: http://store.3drobotics.com/products/px4-fmu-flight-management-unit-plus-ioar-ar-dot-drone-adapter-board-kit

-------------------------------------------------------------------------------

## Radio Modems

The 3DR Radio, by 3D Robotics, is the recommended radio modem system. It
is available in the 915MHz spectrum for unlicensed use in the US, as well
as the 433MHz spectrum.

3DR Radio kit [from 3D Robotics][3drradio_store].

[3drradio_store]: http://store.3drobotics.com/products/3dr-radio-telemetry-kit-915-mhz 

-------------------------------------------------------------------------------

## Hobby Radio Controller

Many hobby radio transmitters and receivers will work with the SMACCMPilot
system: the primary requirement is that the receiver support PPM output.

One inexpensive system which works well is the Turnigy 9XR Transmitter with
FRSKY digital radio modules.

The Turnigy 9XR is available [from Hobby King][t9xr_store].

The FRSKY DJT - JR tramsitter module fits the Turnigy 9XR system.
The FRSKY D4R-II receiver is compact and supports PPM output.

DJT-JR Transmitter module available [from Aloft Hobbies][djt_store].

D4R-II Receiver available [from Aloft Hobbies][d4rii_store].

You will need a single female to female servo extension cable to connect the
receiver to the PX4IOAR's PPM input port. (Note that most servo extension
cables sold have one male and one female connector, and you need two female
connectors.)

Appropriate servo extension cable available [from 3D Robotics][servoext_store].

[t9xr_store]: http://hobbyking.com/hobbyking/store/__31544__Turnigy_9XR_Transmitter_Mode_2_No_Module_.html
[djt_store]: http://www.alofthobbies.com/jr-transmiter-telemetry-module.html
[d4rii_store]: http://www.alofthobbies.com/frsky-d4r-ii.html
[servoext_store]: http://store.3drobotics.com/products/servo-extension-cable-female-female


-------------------------------------------------------------------------------

## AR Drone Airframe

The Parrot AR Drone 2.0 is available from many retailers wordwide.

The Parrot AR Drone 2.0 [on Amazon][ardrone_store].

You will need a Torx T6 screwdriver to disassemble the AR Drone.

Torx T6 screwdriver [on Amazon][torxt6_store].

[ardrone_store]: http://www.amazon.com/Parrot-AR-Drone-Quadricopter-Controlled-Android/dp/B007HZLLOK/ref=sr_1_1?ie=UTF8&qid=1352239769&sr=8-1&keywords=ar+drone+2
[torxt6_store]: http://www.amazon.com/Cellet-Torx-T6-Screw-Driver/dp/B002JSM76G/ref=sr_1_6?s=hi&ie=UTF8&qid=1352239845&sr=1-6&keywords=torx+t6+screwdriver

-------------------------------------------------------------------------------

## Battery and Charger

The Parrot AR Drone comes with a battery and charger.  It is possible to use the
included battery with the PX4IOAR by clipping the battery lead from the AR Drone
motherboard and soldering the wires to the PX4IOAR.

You can purchase extra AR Drone batteries [on Amazon][arbatt_store].

The suggested alternative is to use generic Radio Control Airplane style Lithium
Polymer batteries and faster battery chargers. You will need also need to use a
'Deans Plug' battery connector on the PX4IOAR. If you'll be doing a lot of
flying, this is the recommended solution.

A LiPo battery charger [from Tower Hobbies][charger_store].

A 3-cell 1300mAh LiPo battery [from Tower Hobbies][lipo_store].

A 'Deans Plug' connector, with wires, [from Tower Hobbies][deans_store].

[arbatt_store]: http://www.amazon.com/Parrot-AR-Drone-Battery-LiPo-Replacement/dp/B0041G5Y8W/ref=pd_sim_t_22
[charger_store]: http://www3.towerhobbies.com/cgi-bin/wti0001p?&I=LXAZZS&P=ML
[lipo_store]: http://www3.towerhobbies.com/cgi-bin/wti0001p?&I=LXXMP1&P=ML
[deans_store]: http://www3.towerhobbies.com/cgi-bin/wti0001p?&I=LXHGM0


-------------------------------------------------------------------------------

## JTAG Debugger

SMACCMPilot developers will probably want a JTAG debugger. The Black Magic
Probe Mini is a good debugger for the ARM Cortex-M microcontroller on the
PX4FMU board.

The Black Magic Probe Mini is available [from Transition Robotics][probe_store].
Be sure to get the version which includes cables.

[probe_store]: http://transition-robotics.com/products/black-magic-probe-mini

At the time of this writing, PX4FMU boards do not include a JTAG header. You
will have to buy a header separately and solder it on yourself.

10-pin JTAG header is available
[from Mouser, part number 855-M50-3900542][jtagheader_store].

[jtagheader_store]: http://www.mouser.com

