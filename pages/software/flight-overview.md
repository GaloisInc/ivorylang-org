# Flight Control Software Overview

The SMACCMPilot application and all of the support for the hardware platform are
found in the [SMACCMPilot-STM32F4][smaccmpilot-stm32f4] repository.

### Components

The majority of the SMACCMPilot application is written in the Ivory programming
language and the Tower framework - see [languages](../languages/) for more info.

Additionally, the  SMACCMPilot application depends on external C sources to
interface with with ArduPilot project code (discussed below) and the STM32F4
microcontroller hardware.

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

[ardupilot-project]: http://ardupilot.com
[ardupilot-repo]: http://github.com/galoisinc/ardupilot
