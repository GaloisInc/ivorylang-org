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
is no formal documentation, but there are some [examples][] and 
[haddock documentation][tower-haddock].

[tower-haddock]: http://smaccmpilot.org/haddock/tower/Ivory-Tower.html

### Code Generation Backends

At this time, Tower has [a backend][tower-freertos] for the [FreeRTOS][]
operating system, and [a backend][tower-aadl] for an [AADL][] description of the
system, designed for use with an external code generator. Users who want to use
Tower for embedded systems will want to use the FreeRTOS backend.

[tower]: http://github.com/GaloisInc/tower
[tower-freertos]: http://github.com/GaloisInc/tower/tree/master/tower-freertos
[tower-aadl]: http://github.com/GaloisInc/tower/tree/master/tower-aadl
[FreeRTOS]: http://freertos.org
[AADL]: https://www.sei.cmu.edu/architecture/research/model-based-engineering/aadl.cfm

### Tower Examples

Simple examples and tests of the Tower framework are found in
[`tower-examples`][examples]. The [Simple][] example contains a number of
trivial tasks demonstrating each type of communication primitive.

[examples]: https://github.com/GaloisInc/tower/tree/master/tower-examples
[Simple]: https://github.com/GaloisInc/tower/blob/master/tower-examples/examples/Simple.hs

