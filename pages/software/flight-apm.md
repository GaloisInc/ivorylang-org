# Flight Control: APM Software

The SMACCMPilot flight controller uses several components from the [APM
(ArduPilot Mega) software project][ardupilot-project]. We maintain [a branch of
the ArduPilot repository][ardupilot-repo] with the code required by the
SMACCMPilot build.

ArduPilot's hardware and operating system abstraction, called the `AP_HAL`,
permits us to use ArduPilot libraries as part of our platform.
ArduPilot support for the STM32F4 hardware and FreeRTOS operating system is
implemented in the `AP_HAL_SMACCM` library terms of the `hwf4` library from the
smaccmpilot-stm32f4 library.

[ardupilot-project]: http://ardupilot.com
[ardupilot-repo]: http://github.com/galoisinc/ardupilot
