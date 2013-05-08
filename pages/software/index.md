# Software

## Getting the Software

The SMACCMPilot platform is made with open-source software.

For convenience, we have provided the [SMACCMPilot-Build][smaccmpilot-build]
repository which includes all of the sources required to build SMACCMPilot as a
collection of submodules.

The SMACCMPilot application and all of the support for the hardware platform are
found in the [SMACCMPilot-STM32F4][smaccmpilot-stm32f4] repository.

Some of the hardware support in SMACCMPilot is derived directly from the
[ArduPilot project][ardupilot-project]. We maintain [a branch of the ArduPilot
repository][ardupilot-repo] with the code required by the SMACCMPilot build.

The SMACCMPilot application is written in [the Ivory programming
language][ivory], a domain specific language for generating safe C. In addition
to an overview of [programming in Ivory][ivory-overview], a
[manual][ivory-manual] and some simple [example programs][ivory-examples] are
available. The sources for the Ivory language are [available on GitHub][ivory].

SMACCMPilot uses the [Tower framework][tower] as an [Architectural Description
Language (ADL)][wiki-adl] for composing Ivory programs into connected tasks. An
overview of [using Tower][tower-overview] is available, as well as an [example
program][tower-example]. In the SMACCMPilot project, Tower targets
[FreeRTOS][freertos] as an operating system backend. The ArduPilot code reused
in the SMACCMPilot project is also hosted by FreeRTOS.

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

[wiki-adl]: http://en.wikipedia.org/wiki/Architecture_description_language


