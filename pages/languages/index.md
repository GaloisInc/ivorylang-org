# Languages Overview

The SMACCMPilot project is built using several new domain-specific languages
(DSLs). These new languages are embedded in [the Haskell programming
language][haskell], which means they use Haskell syntax and types, and complete
programs are Haskell values.

[haskell]: http://haskell.org

XXX Put DSL diagram here

Embedded Domain Specific Languages (eDSLs) can be more powerful and easier to
learn than standalone DSLs because the host language (Haskell) also serves as a
macro language.

### Ivory Language

The Ivory Language is an eDSL for safe systems progamming. You can think of
Ivory as a safer C, embedded in Haskell.

Ivory Resources:

* [Language overview](ivory-overview.html)
* [Tutorial](fibwalkthrough.html)
* [Examples][ivory-examples]
* [Language sources][ivory-github]

[ivory-examples]: http://github.com/GaloisInc/ivory/tree/master/ivory-examples/examples
[ivory-github]: http://github.com/GaloisInc/ivory

### Tower Language

The Tower Language is an eDSL for composing Ivory programs into real-time
systems.  Tower programs specify communication channels, tasks, and signal
handlers, and generate Ivory code which implements scheduling and communication
for real-time operating systems.

Tower Resources:

* [Tower overview](tower-overview.html)
* [Language sources][tower-github]

[tower-github]: http://github.com/GaloisInc/tower

