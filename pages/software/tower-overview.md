# Tower Framework Overview

[Tower][tower] is a framework for composing Ivory programs into real-time tasks.

Tower is both a specification language and a code generator. A Tower progaram
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

A Tower `Task` collects the `DataPort`s and `Channel`s 

### Tower Example

A simple example use of the Tower framework is found in
[`Ivory.Tower.Test.FooBarTower`][fbtower]. The example connects three
`Task`s using one `DataPort` and one `Channel`.

You can build the `FooBarTower` example using
the [FreeRTOS code generator example][fbtower-freertos] found in the 
[ivory-tower-freertos][tower-freertos] cabal package.

[fbtower]:http://github.com/GaloisInc/tower/blob/master/ivory-tower/src/Ivory/Tower/Test/FooBarTower.hs
[fbtower-freertos]: https://github.com/GaloisInc/tower/blob/master/ivory-tower-freertos/examples/Main.hs

### Tower Metadata

Tower's specification language is designed to support metadata output. At this
time, Tower supports output to Graphviz dot files, which can be used for
vizualization.

![Graphviz output for the FooBarTower example.](/images/tower-foobar.png)

