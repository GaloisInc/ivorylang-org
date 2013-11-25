# GCS Gamepad support

You can fly SMACCMPilot over the secured communication link using a USB gamepad.

At this time, we support the Logitech F710 and F310 gamepads, on a Linux system.

## Setup

To use a joystick, you will need the driver/kernel support for it.  On a Fedora
machine, execute

```
sudo yum install kernel-modules-extra
sudo yum install joystick
```

There is a guide available
[here](https://www.blakerohde.com/blog/2012/06/gamepads-joysticks-on-fedora-17/)
that may help.

To test your setup, you can follow instructions
[here](http://pingus.seul.org/~grumbel/jstest-gtk/) using `jtest`:

```
jstest --event /dev/input/js0
```
You may wish to test/calibrate with a graphical interface; you can use [jstest-gtk](http://pingus.seul.org/~grumbel/jstest-gtk/).

Additionally, you will need the [Pygame][] library installed for MAVProxy to be
able to recognize the gamepad. If you do not have Pygame installed, you may get
the following error:

```
FIXME
```

[Pygame]: http://TODO/FIXME

## Flying via gamepad

Once your gamepad is properly configured, to enable joystick control, enter
`joystick` into the MAVProxy terminal. This will attach MAVProxy to the gamepad
driver and enable the gamepad commands to be sent to the vehicle.

TODO: show successful load terminal

For gamepad control to be accepted by the vehicle, SMACCMPilot must be in Auto
flight mode. The [RC transmitter][rc-tx] deadman switch and mode switch must be
in the appropriate positions for the flight controller to enter Auto mode.

We have programmed a deadman switch which must be depressed for SMACCMPilot to
accept gamepad inputs. On the Logitech F710, the highlighted button is the kill
switch.

![](../images/joystick.png)

Once you hold the deadman switch, the gamepad joysticks will control altitude,
yaw, pitch, and roll.

TODO: image here showing joystick axes

Flying via the gamepad should be considered unreliable. Characteristics of the
3DR Radio modems and encrypted MAVLink communications

Make sure to have a separate backup pilot using the [RC transmitter][rc-tx] at
all times.

## Troubleshooting

TODO: show what happens if the gamepad is recognized improperly.

You may need to restart the MAVProxy joystick module by entering the command:

XXX

