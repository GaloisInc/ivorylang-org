# Flight Software: Components

The SMACCMPilot flight software's origin was a demonstration replacing individual
compnents of the [Ardupilot flight control software][apm] with components written in
Ivory. It has since evolved to

The system is under active development and the following details are expected to
change. See [future developments][] to get an idea where the flight software is
going.

[apm]: flight-apm.html
[future developments]: flight-future.html

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

