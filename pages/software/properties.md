# SMACCMPilot Properties and Evidence

One of the goals of the SMACCMPilot project is to build a higher-assurance
autopilot and to build languages and tools that make it easier to build other
high-assurance systems.

The following sketches some of the requirements for SMACCMPilot and evidence we
have that those requirements are met.

## Philosophy and Approach

As of November 2013, SMACCMPilot is some 50k LOCs of C code and growing.
(Because the C code is generated, this code is perhaps twice the length of
comparable hand-written code.)
Nonetheless, SMACCMPilot (or any fully-featured autopilot) will be tens of
thousands or more of lines of code.

Our goal is to build an autopilot from-scratch quickly: our (end of) 2013
release represents only about 2 engineer years of development.  Consequently,
verification approaches that require significant manual labor (e.g., interactive
theorem-proving) or that do not scale to large code-bases (e.g., model-checking)
are not practical.  Furthermore, we are taking a "green-field" approach rather
than analyzing existing artifacts for bugs, so we want to do better than the
"bug-hunting" approach found in unsound static analysis.

**Our goal is increased programmer productivity and increased code assurance
  compared to typical embedded development projects.**

Finally, our focus is to use open-source verification tools for our open-source
project: we want the approaches developed here to be available to everyone
interested in high-assurance software development.

## Software Development Approach

### Code Generation
SMACCMPilot is developed using [Ivory and Tower](../../languages/index.html),
two domain-specific languages developed under the project.  These languages
improve programmer productivity but also ensure correctness properties *by
construction*.

Ivory is designed to be expressive while providing evidence that the generated
code does not contain undefined behaviors and memory-safety violations.

In addition, Ivory prevents various constructs that might be well-defined but
often make code analysis more difficult, lead to bugs, or both.  For example,
the following properties hold for Ivory-generated code:

- No heap allocation.  All memory allocation is on the control stack.
  All memory allocation is of statically-known sizes.
- User-code loop iterations are bounded by a constant.
- No machine-dependent integer types.
- Expressions have no side-effects.
- No pointer arithmetic.
- Type casts are limited to information-preserving casts or casts that
  require a default value if truncation may occur (e.g., casting from a signed
  to unsigned value).

These kind of rules are reminiscent of JPL's
[Power of 10](http://spinroot.com/gerard/pdf/Power_of_Ten.pdf) rules.

The Ivory compiler optionally instruments the generated C code with assertions.
Current assertions checks are implemented for

- integer underflow/overflow
- division-by-zero
- floating point infinity/not-a-number results

We compile with `-Wall`.  Note that we in effect compile with `-Wall` twice:
once with our Haskell compiler (since Ivory is an *embedded* domain-specific
language in Haskell), and once with our C compiler.

Additionally, we run the [cppcheck](http://cppcheck.sourceforge.net/) lint tool on our generated (and
hand-written) C and C++.

Tower lifts the level of abstraction in developing inter-communicating tasks
(including device drivers).  Tower helps to prevent

- The incorrect use of RTOS system calls.
- Synchronization and deadlock bugs.
- Unintended shared memory or global memory between tasks.

### Testing
During development and internal deployment, we always execute code with assertions
turned on always.

We have also implemented a
[QuickCheck](http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html) harness
for Ivory.  We use QuickCheck to test some portions of SMACCMPilot, easily
generating 100s of thousands of calls to C functions with random arguments.

We plan to extend our use of QuickCheck to more of SMACCMPilot in the future.

### Static Analysis

SMACCMPilot is analyzed by [Coverity](https://scan.coverity.com/projects/1420).

<a href="https://scan.coverity.com/projects/1420">
  <img alt="Coverity Scan Build Status"
           src="https://scan.coverity.com/projects/1420/badge.svg"/>
		            </a>

In addition, we have been used the open-source C model-checker
[CBMC](http://www.cprover.org/cbmc/).  CBMC is an excellent model-checker for
verifying C source code, and we have found bugs in SMACCMPilot using it.

However, it no longer scales due to the size of the code-base since it performs
whole-program analysis.

We are currently in the process of writing a domain-specific model-checker
customized to Ivory which we hope will scale better.  Until then, there may be
latent unchecked integer under/overflows or divisions-by-zero.

### Runtime Verification

We have built a *runtime verification* framework. It automatically instruments C
code to capture global state values whenever a variable changes.  The framework
is built using a combination of [GCC plugins](http://gcc.gnu.org/wiki/plugins)
and Ivory macros for generating monitoring code based on [past-time linear
temporal logic](http://fsl.cs.illinois.edu/index.php/Past_Time_Linear_Temporal_Logic)
assertions.

The framework is primarily intended for monitoring (and responding to failures)
in untrusted C code.

## Trusted and Untrusted Artifacts

SMACCMPilot consists of a large (and growing) number of libraries and
application code.  Here we briefly itemize the status of major components of the
architecture in terms of their assurance and what a vulnerability might mean for
the overall system.

- *Radio firmware*: The 3DR radio firmware is currently written in C/assembler and
   should be considered to be not only untrusted but potentially buggy.

- *Ground control station* (GCS): The GCS software has no particular assurance
   built into it.  See below for more about cryptography and GCS communications.

- *ArduPilot libraries*: SMACCMPilot currently relies on a small (and
   decreasing) amount of ArduPilot code for sensor input and filtering.

- *Board support package*: SMACCMPilot relies on C drivers to interact with the
   PX4 hardware.  These are being replaced with Ivory-implemented drivers, where
   possible.

- *libc*: SMACCMPilot applications link to libc.

- *Build tools*: of course, we rely on compilers like GCC, GHC (Haskell
  compiler), Make and linker scripts, all of which could introduce bugs.  (If
  you are not afraid of compiler bugs, consider how many GCC bugs have been
  discovered by [Csmith](http://embed.cs.utah.edu/csmith/)).

- *MAVLink*: SMACCMPilot currently uses the
   [MAVLink](http://qgroundcontrol.org/mavlink/start) protocol to communicate
   with the GCS.  Parsers and serializers are generated from XML specifications
   using a Python code generator.  The parsers and serializers that are
   generated are Ivory programs, so they will have the
   memory-safety/well-defined properties that all Ivory programs have, but they
   may have logical bugs.

- *RTOS*: see below.

- *crypto libs*: see below.

### RTOS Assurance

The publicly available version of SMACCMPilot is built on
[FreeRTOS](www.freertos.org), a small, open-source, real-time operating system.
FreeRTOS is written in C and is well tested and well documented; our experience
is that it is highly-reliable.

A (closed-source) fork of SMACCMPilot is built on NICTA's
[eChronos](http://ssrg.nicta.com.au/projects/TS/echronos/) RTOS.  eChronos is
formally-verified, in much of same spirit as NICTA's
[L4 Verified](http://www.ertos.nicta.com.au/research/l4.verified/)  project.

### GCS Security

We have developed an
[end-to-end encryption system](gcs.html#communication-protocol-and-security) for
ground control system (GCS) communication.  The design is based on a
well-defined and studied standard, [AES in Galois/Counter mode][aesgcm-wiki],
and the implementations are built on top of open-source implementations in C and
Haskell.

We do not currently have a key-distribution scheme implemented, so SMACCMPilot
communication has all of the risks associated with using stale, leaked, or
default keys.

The encryption/authentication SMACCMPilot firmware executes as a RTOS
task, in the same address space as all of the other firmware.  Thus, if an
attacker gains access to the firmware binaries or discovers a memory safety
vulnerability that allows her to read/overwrite arbitrary addresses, she can
read/write the private keys.

However, assuming the following should hold of SMACCMPilot:

- All data sent to the GCS by SMACCMPilot is encrypted.
- All data received from the GCS is decrypted and authenticated, and if either
  fails, the message is ignored by the system.
- Only MAVLink messages that SMACCMPilot is configured to process and that are
  well-formed can affect SMACCMPilot's behavior.
- Furthermore, a MAVLink message can only affect the portion of SMACCMPilot's
  behavior it is defined for.

## Miscellaneous Vulnerabilities

- We do not address against sensor spoofing attacks (and GPS spoofing in
  particular).  We do not currently validate GPS input streams. These
  unvalidated streams affect the inertial navigation system, so, we expect a
  GPS spoofing attack could compromise the ability to fly.
- We do not address radio-link bandwidth saturation.
- We do not address hardware/sensor vulnerabilities or faults.
- We do not validate user input to ensure a pilot's behavior is safe.  If you
  try to fly the UAV into a tree, it will fly into a tree.

## Functional Correctness

We have not addressed functional correctness of SMACCMPilot, except for small
portions of the system, such as validating user inputs from the radio
controller.  Moreover, we have not developed a specification of autopilot
correctness; doing so would be a research project itself.

## Future Work

The following are project goals related to increasing the assurance of
SMACCMPilot:

- Software-in-the-loop testing, particularly using Clang-compiled code and LLVM
  analysis tools.
- Domain-specific model-checking.
- Additional QuickCheck testing.
- Worst-case execution time analysis.
- Stack usage analysis.
- Control-flow analysis (to discover recursion).
- Continue reducing the dependence on hand-written C/C++ code.
- Controller/hybrid-system verification.

[aesgcm-wiki]: http://en.wikipedia.org/wiki/Galois/Counter_Mode
