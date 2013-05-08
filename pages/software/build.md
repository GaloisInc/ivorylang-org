# Building SMACCMPilot

These instructions assume you've completed the instructions for [installing
prerequisites][prereqs].

[prereqs]: /software/prerequisites.html

## Fetching Source Dependencies

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
For example, you can test to make sure the code generator for SMACCMPilot exists
and runs with the command

```
./cabal-dev/bin/smaccmpilot-get --help
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

### Continue to [loading SMACCMPilot on PX4][uploading].

[uploading]: /software/uploading.html
