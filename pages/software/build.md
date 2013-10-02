# Building SMACCMPilot

*IMPORTANT:* These instructions assume you've completed the instructions for [installing
prerequisites][prereqs].

[prereqs]: prerequisites.html

## Fetching Source Dependencies

The source code for the SMACCMPilot project is available as open source on
Github. You'll need the source to build and use SMACCMPilot: it is not
distributed as a pre-built binary.

<p><a class="btn btn-primary"
      href="http://github.com/galoisinc/smaccmpilot-build">
    SMACCMPilot-build repository &raquo;</a>
</p>

The [smaccmpilot-build][] repository contains all of the sources you will need
as git submodules. It also contains the FreeRTOS 7.1.0 release as sources, and a
build script to create a cabal-dev sandbox for the SMACCMPilot build.

[smaccmpilot-build]: http://github.com/galoisinc/smaccmpilot-build

First, clone the build repository and fetch the submodules inside.

```
git clone git://github.com/GaloisInc/smaccmpilot-build.git
cd smaccmpilot-build
git submodule init
git submodule update
```

## Building

### Haskell Build

From the `smaccmpilot-build` root directory, use the included Makefile to build
the SMACCMPilot Haskell sources into code generating executables:

```
make
```

The first full Haskell build will take some time to fetch and build dependencies
from Hackage. Subsequent builds will be much faster.

The resulting code generating executables will be found in `./cabal-dev/bin/`.
For example, you can test to make sure the code generator for the SMACCMPilot
flight code exists and runs with the command

```
./cabal-dev/bin/flight-gen --help
```

### C Build

At this point, all of the Haskell sources, including any components written in
Ivory, have been built into executables which will generate C code. The
SMACCMPilot C Build will run these code generators and build the resulting C
sources, along with static (non-generated) C sources.

Change into the smaccmpilot-stm32f4 subdirectory:

```
cd smaccmpilot-stm32f4
```

Before your first build, you'll need to create a `Config.mk` file.

```
cp Config.mk.example Config.mk
```

If you have installed the arm-gcc-embedded toolchain in your PATH, you can now
build. If not, customize the variable `CONFIG_CORTEX_M4_PREFIX` in your
`Config.mk`.

Then, you can run the code generators and build the C sources into executables:

```
make
```

When the build completes, built artifacts will be found in
`build/px4fmu17_ioar_freertos/img/`. These will include complete executables
(elf format, no file extension), stripped binaries (.bin extension), linker
scripts (.lds extension), linker maps (.map extension), and PX4 bootloader
compatible binaries (.px4 extension).

The `flight` binary is the primary flight application for the SMACCMPilot project.
Other applications, like `flight-hil` (HIL stands for Hardware In the Loop), are used
to test subsets of the flight application or individual libraries.

### Troubleshooting

If you get complaints about your *local* gcc toolchain, it is probably related
to the SMACCMPilot Runtime Verification (RTV) build. The RTV build needs to
create a 32-bit plugin for GCC 4.7. Common problems when building it include
using GCC 4.6 or 4.8 (both unsupported, due to changes in the GCC Plugin API),
not having GCC plugin development headers installed, or not having a 32 bit
native toolchain installed on a 64 bit system.

Presently, the RTV build is not required for the flight application build.
You may comment out the `CONFIG_BUILD_RTV` variable your `Config.mk` to disable
the RTV build:

```sh
# ------------------------------------------------------------------------------
# Comment the following line to disable building apps that use runtime
# verification:
CONFIG_BUILD_RTV := 1

```

#### More Details

For more details on how the SMACCMPilot build system works, there is a [document
in the git repository][build-doc] explaining the specifics of the project
Makefiles.

### Continue to [loading SMACCMPilot on PX4][loading].
[loading]: loading.html
[build-doc]: http://github.com/GaloisInc/smaccmpilot-stm32f4/blob/master/doc/build-system.md

