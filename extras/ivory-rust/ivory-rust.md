% Does Ivory Rust?
% Lee Pike, Galois Inc.
% October 2014

Synopsis: We compare and contrast two recently-developed "Safe-C" programming
languages, *Ivory* and *Rust*.

# Introduction

Around 2010, researchers at the University of Washington and the University of
California, San Diego, demonstrated their ability to hack into all the software
systems on a modern automobile, evening doing so remotely (through Bluetooth,
the telemetry system, etc).[^car] While the work was novel, the methods were
not. Most of the vulnerabilities exploited have at their heart some well-known
software vulnerability, such as a memory-safety violation or integer
overflow. The vulnerabilities are particularly found in low-level languages like
C and C++ that mostly rely on the user to write correct programs rather than
enforcing correctness.

Memory safety bugs in C/C++ are mostly obviated in higher-level languages, like
Haskell or Java. High-level languages typically restrict unsafe behaviors as
well as automatically perform some tasks that users might do incorrectly (e.g.,
deallocate memory).

So why are C/C++ still used? There are four reasons:

1. *Legacy concerns*. A lot of code exists already that is C/C++ and a lot of
   developers have specialized in these languages. Furthermore, for some
   embedded systems, compilers for high-level languages do not exist.

2. *Space*. To automate some of unsafe tasks, high-level languages often have a
   runtime system, which can be quite large. For example, the smallest compiled
   Haskell program is on the order of 1MB in size, and requires 1MB of
   random-access memory. So the tiniest program is too large for many
   microcontrollers. In comparison, an entire autopilot written in C/C++ runs on
   a microcontroller containing 8K of RAM and 256K of flash.[^ardupilot]
   Moreover, predicting the space usage of a compiled high-level language is
   difficult and is dependent on the behavior of the runtime system.

3. *Time*. Timing concerns cover both predictability and speed. Efficiently
   written C is still the golden-standard for speed. For real-time systems,
   predictability is also important; the runtime system may unpredictably pause
   the user program to perform garbage collection, for example, causing jitter
   in the program.

4. *Low-level programming*. Finally, without enforced restrictions on pointer
   use and type-casting, C/C++ can simplify interactions with hardware say, when
   writing device drivers. One might argue that hardware interaction is
   inherently unsafe, so why care about safe languages.

# "Safe C" Languages

By *safe C* languages, we mean languages that are advertised to be safer than C,
that are not dependent on a tiny---if any---runtime, and that are designed to
replace C for some subset of applications commonly written in C because of C's
advantages mentioned above.

A handful of languages have been created that provide many of the benefits of C
while providing better safety. Most approaches have been academic, so while the
ideas live on and are incorporated in other languages and compilers, the
implementations have mostly bit-rotted. Li compares some safe C implementations
(the document is a decade old but useful for historical context.[^li]

**Rust**: Rust began as a personal project of a Mozilla employee and was officially
sponsored by the company starting in 2009. The language was influenced by a
large number of languages, notably including many functional programming
languages.

Rust is focused on two primary goals: concurrency and memory-safety, while still
providing the user low-level control of memory and good performance. The
language guarantees memory-safety through static type checking rather than a
runtime system. Therefore, Rust programs are nearly as fast and predictable with
as small memory footprint as C programs.

**Ivory**: Ivory was developed at Galois, Inc. as part of their contribution to
the DARPA HACMS program.[^hacms] Development began in 2012, researching the use
of functional programming techniques for real-time security-critical embedded
systems code. Notably, Ivory is implemented as an *embedded domain-specific
language* (EDSL), meaning that the language is embedded within a host
language. The host language used for Ivory is Haskell[^haskell]. An EDSL
implementation is useful for quickly bootstrapping a compiler, since various
aspects (e.g., a front-end, portions of a type-checker, etc.) can be borrowed
from the host language. Furthermore, the host language acts as a Turing-complete
type-safe macro language.

The main application written in Ivory is a full-featured autopilot called
SMACCMPilot,[^smaccmpilot] which is approximately 50k lines of Ivory code. The
Ivory code implements the most of the software stack including device drivers
and board support, flight controllers, the communications subsystem.

The current backend target for Ivory is C99, to enable linking with C libraries
and to use C-based cross-compilers.

# Comparing Rust and Ivory

Rust is one of the most advanced, safe C languages being developed, both in
terms of its technology but also in terms of its community and openness. Rust is
currently a measuring stick against which to compare other safe C languages.

In the following we compare Rust and Ivory along a few dimensions that are
important to embedded programming. The comparisons are with respect to the
implementations of the respective languages as of October 2014.

While we strive for fairness, I am an Ivory developer and user and have used
Rust only for small examples. I do not have insight into the direction of Rust
developments. Another bias includes the categories themselves: some may be more
relevant to Ivory's use-cases.

That said we compare Rust and Ivory on the following dimensions: concurrency,
memory manipulation, arithmetic, programming paradigms, macro programming,
contracts/testing/formal verification, compiler implementations, adoption,
maturity, and legacy.

## Concurrency

**Rust**: Rust focuses on providing concurrency (the language was inspired by
writing fast, long-running server applications). The concurrency model is
similar to Erlang's model. Lightweight tasks use message passing over
channels. Channels are statically typed. Tasks are spawned at runtime and
error-handling is provided. Rust provides both lightweight and OS threads.

Rust's concurrency framework is provided as a library, and the developers even
envision other concurrency libraries, with specialized semantics, might be
developed in the future.

**Ivory**: Ivory does not natively provide concurrency primitives, and also
provides concurrency via a library. Ivory's concurrency library is called
*Tower*.[^tower] Tower provides scheduling and communication primitives for
real-time system concurrency. Tower requires a small set of runtime system
primitives to be provided---essentially a scheduler, mutexes, priorities, and a
clock. Tower can currently use two real-time operating systems,
FreeRTOS[^freertos] and eChronos,[^echronos] as runtime systems.

Tower has statically-typed channels. Tasks are static: they all are initialized
when the program starts and no task is stopped before the program
terminates. Tasks can be periodic or aperiodic (aperiodic tasks are primarily
used to implement interrupt service routines). Synchronous tasks (i.e.,
callbacks) are supported. Synchronous tasks derive their schedule from their
caller(s) schedules.

## Memory manipulation

**Rust**: Rust has a powerful type system to enforce memory safety while allowing
the programmer to write expressive heap-based data structures without requiring
significant runtime support.

Rust provides reference counting for garbage collection as a library that can be
optionally used. Other garbage collecting libraries may be developed in the
future.

Rust contains an affine type system that guarantees that memory cannot be
manipulated through more than one reference at a time. This prevents aliasing
errors, which are particularly difficult to discover in concurrent programs but
also problematic in single-threaded programs.

Rust provides both product (record) types and sum types, together with
pattern-matching facilities.

**Ivory**: Ivory currently disallows heap-based data structures, as is often
encouraged for safety-critical embedded systems.[^jpl] Thus, Ivory is unsuitable
for some programs with dynamic memory requirements (e.g., servers). All dynamic
allocation in Ivory is on the stack. In the future, memory region memory
management may be added.

The only reference aliasing in Ivory is implicit, e.g., when two function
arguments are the same parameter.

Ivory also does not have sum types, although they can be approximated to some
extent using macros.

Ivory also provides bit-data facilities, useful for writing device drivers.[^bitdata]

## Arithmetic

Low-level programs operate over bounded integer types (e.g., signed 32-bit
integers, unsigned 8-bit integers, etc.). Arithmetic overflow can cause
unexpected but defined behavior (e.g., `1+255 == 0` in unsigned 8-bit
arithmetic). In C, signed integer overflow is undefined. So is division-by-zero,
bit-shifts on signed types, left bit-shifts by a negative value, etc.

**Rust**: Rust currently does not focus on providing any support for detecting or
preventing undefined behaviors caused by undefined arithmetic. For example, the
following Rust program produces no warnings during compilation but aborts during
execution:

```rust
fn main() {
  let x = 3/0i;
  println!("{}", x);
}
```

In the future, it may be possible for Rust to build on LLVM analyses.

**Ivory**: Discovering undefined arithmetic behavior is undecidable in
general. However, simple static analyses in the compiler can detect some
undefined behavior. For example, during compilation, the following Ivory
program detects division-by-zero:

```c
int32_t main() {
  let x = 3/0;
    printf(x);
    }
```

Furthermore, if `x` is instead assigned the expression `127+1` and `x` is a
signed 8-bit integer, then a compilation error is also triggered.

For arithmetic expressions that cannot be validated at compile time, the Ivory
compiler can insert assertions that perform a user-specified action when an
undefined arithmetic expression is evaluated. These assertions can be used
during testing or with Ivory's associated formal verification tools (see
the section entitled, "[Contracts, testing, and formal verification]").

Ivory tries to avoid implementation-dependent behavior as much as possible to
simplify porting between different architectures. For example, there is no `int`
type. All integer types must be specified as signed or unsigned with their
bit-width (e.g., `uint64`, `int8`, etc.).


## Programming paradigms

**Rust**: Rust supports object-oriented and functional programming models,
including closures and generics (i.e., parametric polymorphism), algebraic
datatypes, and pattern matching.

**Ivory**: As an EDSL, Ivory is on the one-hand, just Haskell, so it is entirely
within the functional programming paradigm. The Ivory language is monadic, so it
"feels" procedural, however. Ivory's concrete syntax is procedural.

Ivory has an experimental "concrete syntax" that allows the user to write
C-style programs that get parsed into Haskell (as shown in the "[Arithmetic]"
section).

## Macro programming

**Rust**: Rust supports macro programming. Macros are hygienic.

**Ivory**: As an EDSL, Ivory macros (i.e., regular Haskell) are in some ways the
 primary way to program. Most of the standard library is written as
 macros. Macros are used to implement many of the features not included in Ivory.

## Contracts, testing, and formal verification

**Rust**: Currently, Rust provides a "test" attribute for writing and running
tests in the source code. More sophisticated testing frameworks are
planned. Rust currently has no built-in verification tools. A QuickCheck port to
Rust is fairly mature.[^qc]

**Ivory**: Ivory allows the user to write inline assertions, and functions can
be decorated with pre and post conditions. The compiler can optionally insert
assertions that are verification conditions for ensuring the lack of undefined
behavior.

Ivory comes with a version of QuickCheck for random test-case generation. Ivory
currently has two experimental verification back-ends, including a symbolic
simulator that generates CVC4[^cvc4] (an SMT solver) assertions, and one that
generates ACL2[^acl2] (an automated theorem prover) assertions.

Because Ivory generates simple conforming C99, C analysis tools perform well on
generated Ivory code. We have used Coverity[^coverity] to analyze the
Ivory-generated autopilot.[^coverity-smaccm]

Ivory is comparible to SPARK/Ada[^spark] with respect to integrated development
tools.

## Compiler implementations

**Rust**: Rust's compiler is self-hosted in Rust. The compiler is around 300k
 lines of code. The backend is LLVM.[^llvm]

**Ivory**: Ivory's compiler is written in Haskell. The current Ivory compiler,
verification tools, and standard libraries are just over 20k lines of code. The
current backend is.

## Adoption

**Rust**: Rust is open-source and has over 600 contributors. It appears that
 language will continue to grow in usage and popularity at a rapid pace. The
 main impediment to more widespread usage currently appears to be the lack of
 stability in the language and the lack of libraries.

**Ivory**: Ivory is open-source, but the development team is small (less than 10)
 and mostly in-house at Galois. There is a small number of users outside of
 Galois. As an EDSL, it will likely find its greatest adoption among Haskell
 developers who wish to write low-level code. If Ivory becomes a stand-alone
 compiler, the user-base would likely remain small, given the domain of focusing
 on high-assurance embedded systems.

## Maturity

**Rust**: The language is nearing a 1.0 release, at which point certain features
become stable. Despite it's popularity already, Rust is a very fast-moving
language, with many changes to its syntax and semantics. Some portions of the
compiler are quite immature (e.g., sanitization passes for undefined behavior),
while others, like type-checking, are quite mature.

**Ivory**: The core Ivory EDSL, including the syntax and semantics is stable, as
is the C backend. The associated tools and libraries are less stable. Integrated
testing of the compiler toolchain, documentation, etc. are not mature.

## Legacy

**Rust**: Rust might be the most ambitious project with the greatest chance of
success to-date to bring memory-safety and functional programming constructs to
the world of embedded programming. The language and its community is in its
infancy but it is strong and growing. Rust competes directly with C, C++, and
Go. It competes indirectly with Swift (given Swift's focus on Objective C
interoperability).

**Ivory**: Some of Ivory's most unique features relate to it being implemented as
an EDSL. As a feature-rich EDSL, Ivory serves as a case-study for the benefits
and detriments of using EDSLs in the large.[^icfp] With respect to EDSLs, most
of Ivory's domain-specific types are embedded into Haskell general-purpose
types, relying on many of Haskell's dependent-typing extensions.

Ivory's niche is quite limited, focusing on very high-assurance embedded
software systems.

## Conclusions

Both Ivory and Rust are new languages exploring different portions of the design
space for efficient, safe, low-level programming, and are encouraging
alternatives to C and C++.

Rust's strengths are its concurrency system, its rich type system to enforce
safe use of pointers, functional programming constructs built into the language,
and of course, its community momentum.

Ivory's strengths are its embedding in a popular functional language, the
simplicity of the compiler, and formal methods tools integration.

## Acknowledgements

Trevor Elliott, Pat Hickey, Eric Seidel, Jamey Sharp, Getty Ritter, Jon
Sterling, and Darin Morrison provided helpful feedback on this document. Errors
are mine alone.


[^ardupilot]: https://code.google.com/p/ardupilot-mega/wiki/ProgrammingArduino
[^hacms]: http://www.darpa.mil/Our_Work/I2O/Programs/High-Assurance_Cyber_Military_Systems_(HACMS).aspx
[^car]: http://www.autosec.org/publications.html
[^rust]: <http://www.rust-lang.org/>
[^ivory]: http://ivorylang.org/
[^tower]: http://www.cs.indiana.edu/~lepike/pub_pages/icfp14.html
[^freertos]: http://www.freertos.org/
[^echronos]: http://ssrg.nicta.com/projects/TS/echronos/
[^icfp]: http://www.cs.indiana.edu/~lepike/pub_pages/icfp14.html
[^haskell]: https://www.haskell.org/haskellwiki/Haskell
[^smaccmpilot]: http://smaccmpilot.org/
[^jpl]: http://spinroot.com/gerard/pdf/P10.pdf
http://en.wikipedia.org/wiki/The_Power_of_10:_Rules_for_Developing_Safety-Critical_Code
[^llvm]: http://llvm.org/
[^acl2]: http://www.cs.utexas.edu/users/moore/acl2/
[^cvc4]: http://cvc4.cs.nyu.edu/web/
[^qc]: http://en.wikipedia.org/wiki/QuickCheck
[^coverity]: https://scan.coverity.com/
[^coverity-smaccm]: https://scan.coverity.com/projects/1420
[^li]: http://www.cis.upenn.edu/grad/documents/li.pdf
[^go]: https://golang.org/
[^swift]: https://developer.apple.com/swift/
[^spark]: http://en.wikipedia.org/wiki/SPARK_(programming_language)
[^bitdata]: http://web.cecs.pdx.edu/~mpj/pubs/bitdata-icfp05.pdf
