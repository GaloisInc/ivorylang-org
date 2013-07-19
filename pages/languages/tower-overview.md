# Tower Language Overview

[Tower][tower] is a language for composing Ivory programs into real-time tasks.

Tower is both a specification language and a code generator. A Tower program
describes communication channels and tasks, and provides an Ivory
implementation of each task. Tower compiles the specification for the program
and delegates code generation to an operating-system specific backend.

At this time, Tower has [a single backend][tower-freertos] for the [FreeRTOS][]
operating system.

[tower]: http://github.com/GaloisInc/tower
[tower-freertos]: http://github.com/GaloisInc/tower/tree/master/ivory-tower-freertos
[FreeRTOS]: http://freertos.org

### Tower Concepts

The basic communication primitives of Tower are `DataPort` and `Channel`,
which implement shared state and first-in first-out queues, respectively.

A Tower `Task` is a collection of event handlers which share common state and
are cooperatively scheduled. You can think of a task as a `select` loop which
demultiplexes incoming events into assigned handlers. Event sources can be a
timer tick, scheduled on a fixed period using the `onPeriod` handler, or a new
value available on a `ChannelSink`, using the `onChannel` handler.

A Tower `Signal` is a single handler for an asynchronous event. `Signal`s are
designed for implementing Interrupt Service Handlers (ISRs) on embedded systems.
Signals are restricted to using `Channel` primitives, and cannot access
`DataPort`s.

### Tower Example

A simple example use of the Tower framework is found in
[`Ivory.Tower.Test.FooBarSimple`][fbsimple]. The example connects three
`Task`s using one `DataPort` and one `Channel`.

You can build the `FooBarSimple` example using
the [FreeRTOS code generator example][fbsimple-freertos] found in the 
[ivory-tower-freertos][tower-freertos] cabal package.

Tower also supports signal handlers, which may be used to implement ISR routines
in embedded system. The [`Ivory.Tower.Test.FooBarSignals`][fbsimple] example builds
on the FooBarSimple example, adding channels between `Task`s and `Signal`s.

[fbsimple]:http://github.com/GaloisInc/tower/blob/master/ivory-tower/src/Ivory/Tower/Test/FooBarSimple.hs
[fbsimple-freertos]: https://github.com/GaloisInc/tower/blob/master/ivory-tower-freertos/examples/Main.hs
[fbsignals]:http://github.com/GaloisInc/tower/blob/master/ivory-tower/src/Ivory/Tower/Test/FooBarSignals.hs

### Tower Metadata

Tower's specification language is designed to support metadata output. At this
time, Tower supports output to Graphviz dot files, which can be used for
visualization.

![Graphviz output for the FooBarTower example.](/images/tower-foobar.png)

Graphviz output legend:

* Tasks and Signals are rectangular nodes with fields specifying name, special
  properties, and all data and event sources and sinks.
* DataPorts are elliptical nodes specifying the data's Ivory type. Dotted arrows
  show DataPort reads and writes.
* Channels are solid arrows specifying the channel's Ivory type.
