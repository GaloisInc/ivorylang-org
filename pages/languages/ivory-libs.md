# Ivory Language: Libraries

Libraries included with the Ivory language repository:

* [`ivory-stdlib`][ivory-stdlib]: common operators and control structures
* [`ivory-bitdata`][ivory-bitdata]: tools for bit-precise operations
* [`ivory-hw`][ivory-hw]: hardware IO support

## Standard Library

The [`ivory-stdlib`][ivory-stdlib] package contains many useful operators and
control structures which are not part of the Ivory language. It is the policy of
the Ivory language designers to only export primitives to the user. Useful
functions which are implemented in terms of Ivory primitives are found in the
standard library.

## Bit Data

The [`ivory-bitdata`][ivory-bitdata] package is an interface for implementing
safe bit-precise operations on top of the Ivory core language. This library
provides a quasiquoter for specifying the layout of bits in registers and
a typed interface for modifying register contents.

See [some][bitdata-ex1] [examples][bitdata-ex2] of the bitdata library in the
[`ivory-bsp-stm32f4`][ivory-bsp-stm32f4] package.

## Hardware IO Support

The [`ivory-hw`][ivory-hw] package is the basis for hardware IO in Ivory.

`ivory-hw` uses external C definitions unsafe primitives writing to arbitrary
memory locations. Safety is provided by checks that memory access is in bounds
at code generation time. Memory bounds are defined statically in
`Ivory.HW.STM32F4` according to the STM32F4 microcontroller memory map.

We are investigating solutions to a generalized safe `ivory-hw` library,
parameterized by memory map rather than statically bound to a particular
architecture.

[ivory-stdlib]: http://github.com/GaloisInc/ivory/tree/master/ivory-stdlib
[ivory-hw]: http://github.com/GaloisInc/ivory/tree/master/ivory-hw
[ivory-bitdata]: http://github.com/GaloisInc/ivory/tree/master/ivory-bitdata

[bitdata-ex1]: http://github.com/GaloisInc/smaccmpilot-stm32f4/blob/master/src/ivory-bsp-stm32f4/src/Ivory/BSP/STM32F4/SPI/Regs.hs
[bitdata-ex2]: http://github.com/GaloisInc/smaccmpilot-stm32f4/blob/master/src/ivory-bsp-stm32f4/src/Ivory/BSP/STM32F4/SPI/Peripheral.hs
[ivory-bsp-stm32f4]: http://github.com/GaloisInc/smaccmpilot-stm32f4/tree/master/src/ivory-bsp-stm32f4
