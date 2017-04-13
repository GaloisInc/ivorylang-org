# Tower Language Overview

The Tower Language is an eDSL for composing Ivory programs into real-time
systems.  Tower programs specify communication channels, tasks, and signal
handlers, and generate Ivory code which implements scheduling and communication
for real-time operating systems.


Tower is both a specification language and a code generator. A Tower program
describes communication channels and tasks, and provides an Ivory implementation
of each task. Tower compiles the specification for the program and delegates
code generation to an operating-system specific backend.

At the moment, Tower is still under active development and is not stable. There
is no formal documentation, but there are some [examples][examples].

### Code Generation Backends

At this time, Tower has [a backend][tower-freertos] for the [FreeRTOS][]
operating system, a [backend][tower-echronos] for the [eChronos] operating system, and [a backend][tower-aadl] for an [AADL][] description of the
system, designed for use with an external code generator. Users who want to use
Tower for embedded systems will want to use the FreeRTOS or eChronos backend.

[tower]: http://github.com/GaloisInc/tower
[tower-freertos]: https://github.com/GaloisInc/ivory-tower-stm32/tree/master/tower-freertos-stm32
[tower-echronos]: https://github.com/GaloisInc/ivory-tower-stm32/tree/master/tower-echronos-stm32
[tower-aadl]: http://github.com/GaloisInc/tower/tree/master/tower-aadl
[FreeRTOS]: http://freertos.org
[eChronos]: https://github.com/galoisinc/echronos/tree/master
[AADL]: https://www.sei.cmu.edu/architecture/research/model-based-engineering/aadl.cfm

### Tower Examples

Simple examples and tests of the Tower framework are found in
[`tower-examples`][examples]. The [Simple][] example contains a number of
trivial tasks demonstrating each type of communication primitive.

[examples]: https://github.com/GaloisInc/ivory-tower-stm32/tree/master/tower-freertos-stm32-tests
[Simple]: https://github.com/GaloisInc/ivory-tower-stm32/blob/master/tower-freertos-stm32-tests/examples/Simple.hs

