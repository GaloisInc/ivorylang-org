# Flight Software: Components

The SMACCMPilot flight software's origin was a demonstration replacing individual
compnents of the [Ardupilot flight control software][apm] with components written in
Ivory. It has since evolved to

The system is under active development and the following details are expected to
change. See [future developments][] to get an idea where the flight software is
going.

[apm]
[future developments]

## Components

put the system diagram here

### Hardware Support

- hwf4 library
- ap\_hal manages all hardware, sensor io
- no functional eeprom working at this time

### Sensor Fusion

- APM sensor fusion manages all sensor fusion

### Stabilization Algorithm

- trivial pid controller in ivory

### GCS Support

- mavlink implementation in ivory generated from mavlink xml, code generator
- messages implemented

---
content should be rearranged from below:

The majority of the SMACCMPilot application is written in the Ivory programming
language and the Tower framework - see [languages][] for more info.

The  SMACCMPilot application depends on C code to interface with with APM
project code (discussed below) and the STM32F4 microcontroller hardware.

The [`hwf4`][hwf4-dir] library was created for the SMACCMPilot project to provide a
reasonable abstraction of the STM32F4 microcontroller peripherals. We use the
hwf4 library from Ivory code using the [`ivory-bsp-hwf4wrapper`][hwf4-cabal]
package, which imports the hwf4 primitives into Ivory.

[hwf4-dir]: http://github.com/GaloisInc/smaccmpilot-stm32f4/tree/master/bsp/hwf4
[hwf4-cabal]: http://github.com/GaloisInc/smaccmpilot-stm32f4/blob/master/bsp/ivory/ivory-bsp-hwf4wrapper/ivory-bsp-hwf4wrapper.cabal

