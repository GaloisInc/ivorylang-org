# Software

The SMACCMPilot software project is both an innovative open-source flight
controller and the flagship project for the development of a new programming
language.

### Umbrella Repository

For convenience, we have provided the [SMACCMPilot-Build][smaccmpilot-build]
repository which includes all of the sources required to build SMACCMPilot as a
collection of submodules. There are instructions for [building this
repository](/software/build.html).

### Flight Control Software

The SMACCMPilot application and all of the support for the hardware platform are
found in the [SMACCMPilot-STM32F4][smaccmpilot-stm32f4] repository.

The majority of the SMACCMPilot applicaton is written in the Ivory programming
language and the Tower framework. The Haskell package
[`smaccmpilot`][smaccmpilot-cabal], packages the Ivory sources and compiler into
a single Haskell program which, when run, generates C sources for the
application.

[smaccmpilot-cabal]: http://github.com/GaloisInc/smaccmpilot-stm32f4/blob/master/ivory/smaccmpilot.cabal

The SMACCMPilot application depends on external C sources to interface with
with ArduPilot project code (discussed below) and the STM32F4 microcontroller
hardware.

The [`hwf4`][hwf4-dir] library was created for the SMACCMPilot project to provide a
reasonable abstraction of the STM32F4 microcontroller peripherals. We use the
hwf4 library from Ivory code using the [`ivory-bsp-hwf4wrapper`][hwf4-cabal]
package, which imports the hwf4 primitives into Ivory.

[hwf4-dir]: http://github.com/GaloisInc/smaccmpilot-stm32f4/tree/master/bsp/hwf4
[hwf4-cabal]: http://github.com/GaloisInc/smaccmpilot-stm32f4/blob/master/bsp/ivory/ivory-bsp-hwf4wrapper/ivory-bsp-hwf4wrapper.cabal

### ArduPilot Project Software

Some of the hardware sensor support in SMACCMPilot is derived directly from the
[ArduPilot project][ardupilot-project]. We maintain [a branch of the ArduPilot
repository][ardupilot-repo] with the code required by the SMACCMPilot build.

ArduPilot's hardware and operating system abstraction, called the `AP_HAL`,
permits us to use ArduPilot libraries as part of our platform.
ArduPilot support for the STM32F4 hardware and FreeRTOS operating system is
implemented in the `AP_HAL_SMACCM` library terms of the `hwf4` library from the
smaccmpilot-stm32f4 library.

### Ivory Programming Language

The SMACCMPilot project is the flagship project of [the Ivory programming
language][ivory], a domain specific language for generating safe C. Ivory is
[embedded][edsl] in the [Haskell programming language][haskell].

In addition to an overview of [programming in Ivory][ivory-overview], a
[manual][ivory-manual] and some simple [example programs][ivory-examples] are
available. The Ivory language is open source and [available on GitHub][ivory].

### Tower Framework for Ivory

SMACCMPilot uses the [Tower framework][tower] as an for composing Ivory programs
into connected tasks. An overview of [using Tower][tower-overview] is available,
as well as an [example program][tower-example]. In the SMACCMPilot project,
Tower targets [FreeRTOS][freertos] as the operating system.


[ivory]: http://github.com/galoisinc/ivory
[tower]: http://github.com/galoisinc/tower
[smaccmpilot-build]: http://github.com/galoisinc/smaccmpilot-build
[smaccmpilot-stm32f4]: http://github.com/galoisinc/smaccmpilot-stm32f4
[ardupilot-project]: http://ardupilot.com
[ardupilot-repo]: http://github.com/galoisinc/ardupilot

[ivory-manual]: http://github.com/GaloisInc/ivory/blob/master/ivory/user-guide.md
[ivory-examples]: http://github.com/GaloisInc/ivory/tree/master/ivory-examples/examples

[ivory-overview]: /software/ivory-overview.html

[tower-overview]: /software/tower-overview.html
[tower-example]: http://github.com/GaloisInc/tower/blob/master/ivory-tower/src/Ivory/Tower/Test/FooBarTower.hs 

[freertos]: http://freertos.org

[edsl]: http://www.haskell.org/haskellwiki/Embedded_domain_specific_language 
[haskell]: http://www.haskell.org/
