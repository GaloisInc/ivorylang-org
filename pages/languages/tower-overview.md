# Tower Language Overview

[Tower][tower] is a language for composing Ivory programs into real-time tasks.

Tower is both a specification language and a code generator. A Tower program
describes communication channels and tasks, and provides an Ivory implementation
of each task. Tower compiles the specification for the program and delegates
code generation to an operating-system specific backend.

At this time, Tower has [a single backend][tower-freertos] for the [FreeRTOS][]
operating system.

[tower]: http://github.com/GaloisInc/tower
[tower-freertos]: http://github.com/GaloisInc/tower/tree/master/tower-freertos
[FreeRTOS]: http://freertos.org

### Tower Example

Simple examples and tests of the Tower framework are found in
[`tower-examples`][examples]. The [`Simple`][] example contains a number of
trivial tasks demonstrating each type of communication primitive.

[examples]: https://github.com/GaloisInc/tower/tree/master/tower-examples
[Simple]: https://github.com/GaloisInc/tower/blob/master/tower-examples/examples/Simple.hs

### Tower Metadata

graphviz, aadl, and others to come
