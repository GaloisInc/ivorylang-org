# Black Magic Probe

There are many JTAG and SWD debuggers available for the ARM Cortex-M4
processor. We have had good results using the [Black Magic Probe][bmprobe]
from [Black Sphere Technologies][blacksphere].

![Black Magic Probe](../images/blackmagic.jpg)

### PX4 Autopilot Project Guide

As your primary resource, please read the PX4 Autopilot project wiki [guide to
using the Black Magic Probe][guide].

### Upgrading Firmware

We've found that most Black Magic Probes do not ship with the very latest
firmware. In order to support the PX4FMU's STM32F4 processor, you should
please [follow the instructions on the PX4 Autopilot wiki][guide] to clone
and build the latest black magic firmware and load it on your device.

### GDB Init Script

We use a simpler gdb init script from the one described on the PX4 wiki
due to differences in operating system implementation and environment.

Copy the following to a `.gdbinit` script in the root directory of your
`smaccmpilot-stm32f4` repository:

```
target extended SERIAL_PORT
monitor swdp_scan
attach 1
```

where `SERIAL_PORT` is the path of the first serial device enumerated by the
Black Magic probe.

### Using GDB

From the root of the `smaccmpilot-stm32f4` repository, after successfully
[building the SMACCMPilot executable](../software/build.html),
start your ARM toolchain gdb with the `flight` executable.

```
arm-none-eabi-gdb build/px4fmu17_ioar_freertos/img/flight

```

If you've created the `.gdbinit` script above, you are now ready to load and
run the SMACCMPilot application. If not, you may enter those commands
sequentially at the gdb prompt.

Once your gdb is attached to the probe, you can write the program to flash
with the command:

```
(gdb) load
```

Then, to begin execution, use the command:

```
(gdb) run
```

### Other GDB Resources

Your author typically uses [this gdb reference
card](http://www.cs.berkeley.edu/~mavam/teaching/cs161-sp11/gdb-refcard.pdf)
while debugging.


[bmprobe]: http://www.blacksphere.co.nz/main/blackmagic
[blacksphere]: http://www.blacksphere.co.nz/main/index.php

[guide]: http://pixhawk.ethz.ch/px4/dev/jtag/black_magic_probe
