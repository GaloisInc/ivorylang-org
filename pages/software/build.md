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
build script to create a cabal sandbox for the SMACCMPilot build.

[smaccmpilot-build]: http://github.com/galoisinc/smaccmpilot-build

First, clone the build repository and fetch the submodules inside.

```
git clone git://github.com/GaloisInc/smaccmpilot-build.git
cd smaccmpilot-build
git submodule init
git submodule update
```

## Building

### Configuration

You'll need to do a little bit of manual configuration before building
for the first time.

Change into the smaccmpilot-stm32f4 subdirectory:

```
cd smaccmpilot-stm32f4
```

Before your first build, you'll need to create a `Config.mk` file.

```
cp Config.mk.example Config.mk
```

If you have not installed the arm-gcc-embedded toolchain in your PATH, customize
the variable `CONFIG_CORTEX_M4_PREFIX` in your `Config.mk`.

You also need to create a `Keys.mk` file containing the secret keys which will
be used to secure communications between the SMACCMPilot vehicle and ground
control station. You can start by copying the template key file, and optionally
replace the default keys them with values you randomly generate yourself.

```
cp Keys.mk.exmple Keys.mk
```

When complete, change back to the outer smaccmpilot-build directory:

```
cd ..
```

### Top Level Haskell Build

From the `smaccmpilot-build` root directory, use the included Makefile to build
the project:

```
make
```

The first full Haskell build will take some time to fetch and build dependencies
from Hackage. Subsequent builds will be much faster.

Make will build the Haskell sources into code generators, and then use the code
generators to cross-compile the generated C sources into embedded system images.

The resulting code generating executables will be found in `./.cabal-sandbox/bin/`.
For example, you can test to make sure the code generator for the SMACCMPilot
flight code exists and runs with the command

```
./.cabal-sandbox/bin/flight-gen --help
```


### C Build

Developers may want to run the C build separately from the code generation
build. Since the C build is just a recursive `make` call into the
`smaccmpilot-stm32f4` subdirectory, you can build the C sources into executables
with:

```
cd smaccmpilot-stm32f4
make
```

Before your first build, you'll need to create both a `Config.mk` and a
`Keys.mk` file.

The C build output will be found in
`./smaccmpilot-stm32f4/build/{platform}_{os}/`
where `{platform}` is one of:

* `px4fmu17_ioar`: PX4FMU 1.7 with the IOAR expansion board,
        for the  Drone based copter
* `px4fmu17_bare`: PX4FMU 1.7 without an IO expansion board,
        for a radio control ESC based copter like the 3DR ArduCopter Quad

and `{os}` is one of:

* `freertos`: Produces complete images using the FreeRTOS operating system
* `aadl`: Produces applications as libraries, and system description output in
  the Architecture Analysis and Design Language (AADL), for use with other
  operating systems
```
cp Keys.mk.example Keys.mk
```

`Keys.mk` contains the AES symmetric keys, salts, and identifiers that will be
used to communicate between SMACCMPilot and a ground control station (GCS).  The
keys and salts will be compiled into the SMACCMPilot and GCS binaries.  See the
[GCS][gcs] page for more information.

Build artifacts in the `img` subdirectory will include complete executables (elf
format, no file extension), stripped binaries (.bin extension), linker scripts
(.lds extension), linker maps (.map extension), and PX4 bootloader compatible
binaries (.px4 extension).

Generated C code can be found in the `gen` subdirectory.

The `flight` binary is the primary flight application for the SMACCMPilot
project.  Other applications, like `flight-hil` (HIL stands for Hardware In the
Loop), are used to test subsets of the flight application or individual
libraries.

### Troubleshooting

#### Config.mk issues

If you previously built SMACCMPilot successfully but pulling a new version
has broken your build, you may need to update your Config.mk. If you have not
customized Config.mk in the past, you can try `cp Config.mk.example Config.mk`,
or you can run a diff between `Config.mk.example` and your local `Config.mk`

#### RTV build

If you get complaints about your *local* gcc toolchain, it is probably related
to the SMACCMPilot Runtime Verification (RTV) build. The RTV build needs to
create a 32-bit plugin for GCC 4.7. Common problems when building it include
using GCC 4.6 or 4.8 (both unsupported, due to changes in the GCC Plugin API),
not having GCC plugin development headers installed, or not having a 32 bit
native toolchain installed on a 64 bit system.

Presently, the RTV build is disabled by default.

#### More Details

For more details on how the SMACCMPilot build system works, there is a [document
in the git repository][build-doc] explaining the specifics of the project
Makefiles.

### Continue to [loading SMACCMPilot on PX4][loading].

[gcs]: gcs.html
[loading]: loading.html
[build-doc]: http://github.com/GaloisInc/smaccmpilot-stm32f4/blob/master/doc/build-system.md

