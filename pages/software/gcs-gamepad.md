# GCS Gamepad support

You can fly SMACCMPilot over the secured communication link using a USB gamepad.

At this time, we support the Logitech F710 and F310 gamepads, on a Linux system.

![Logitech F710 gamepad](/images/gamepad_top.jpg)

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

You may wish to test/calibrate with a graphical interface; you can use
[jstest-gtk](http://pingus.seul.org/~grumbel/jstest-gtk/).

Additionally, you will need the [Pygame][] library installed for MAVProxy to be
able to recognize the gamepad. If you do not have Pygame installed, you may get
the following error:

```
Unable to load module mavproxy_joystick:
```

[Pygame]: http://www.pygame.org/

## Flying via gamepad

Once your gamepad is properly configured, to enable joystick control, enter
`joystick` into the MAVProxy terminal. This will attach MAVProxy to the gamepad
driver and enable the gamepad commands to be sent to the vehicle.

```
MAV> joystick
Trying joystick 0
joystick found: Logitech Logitech Cordless RumblePad 2
Matched type 'Logitech Logitech Cordless RumblePad 2'
4 axes available
12 buttons available
Loaded module mavproxy_joystick
MAV>
```

For gamepad control to be accepted by the vehicle, SMACCMPilot must be in Auto
flight mode. The [RC transmitter][rc-tx] deadman switch and mode switch must be
in the appropriate positions for the flight controller to enter Auto mode.

We have programmed a deadman switch which must be depressed for SMACCMPilot to
accept gamepad inputs.

![Deadman switch on Logitech F710](/images/gamepad_back.png)

Once you hold the deadman switch, the gamepad joysticks will control altitude,
yaw, pitch, and roll.

TODO: image here showing joystick axes

Flying via the gamepad should be considered unreliable. Transmission errors in
the 3DR Radio modems can prevent joystick commands from reaching the flight
controller. Make sure to have a separate backup pilot using the [RC
transmitter][rc-tx] at all times.

