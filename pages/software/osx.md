# Tips for MacOS X

Most of the instructions for [installing prerequisites](prerequisites.html) and
[building](build.html) should work on at least Linux and OS X, but assume that
you're well acquainted with your operating system's package manager and other
tools. Here are some hints that you may find helpful if you're installing on a
Mac.

This codebase has been built using either [MacPorts][] or [Homebrew][]. The
following instructions are for MacPorts, though we'd welcome documentation
contributions for other setups.

[MacPorts]: http://www.macports.org
[Homebrew]: http://brew.sh

## Installing the ARM GCC Toolchain

You may be tempted to use the MacPorts build of `arm-none-eabi-gcc` rather than
ARM Ltd's toolchain. We don't recommend that route as subtle differences in how
the toolchain is compiled can have surprisingly large effects.

Notably, when testing the MacPorts cross-compiler, it produced this error
message:

```
/opt/local/lib/gcc/arm-none-eabi/4.7.3/../../../../arm-none-eabi/bin/ld: cannot find crt0.o: No such file or directory
```

Instead, we've had success with the MacOS build tarball of [arm-gcc-embedded][]
version 4.8-2014-q2-update.

[arm-gcc-embedded]: http://launchpad.net/gcc-arm-embedded

## Configuring PATH

When you install MacPorts, it adds its `bin` directory to your `$PATH` in
`~/.profile`, but you will also need the path to the ARM GCC toolchain and to
your current version of `cabal`.

If you unpacked arm-gcc-embedded into your home directory, then appending this
line to `~/.profile` should do the trick:

```
export PATH="$HOME/Library/Haskell/bin:$HOME/gcc-arm-none-eabi-4_8-2014q2/bin:$PATH"
```

## Installing GCC for OS X

In addition to the embedded toolchain, you also need a native host toolchain for
compiling the various pieces of Haskell, C, and C++ code in this project.
Apple's Xcode version 5 ships with a program named `gcc`, but it is actually a
shim wrapper around `clang`. Unfortunately, `clang`'s C preprocessor is pickier
about its input than GCC's, which triggers many warnings and may cause your
compile to fail.

You can install a real GCC build and configure the Haskell Platform-installed
GHC to use it.

```
xcode-select --install # needed for /usr/include
sudo port install gcc48
sudo port select --set gcc mp-gcc48
```

Next, edit
`/Library/Frameworks/GHC.framework/Versions/Current/usr/lib/ghc-7.6.3/settings`
and replace `/usr/bin/gcc` with `/opt/local/bin/gcc`.

Now you should have a working GHC installation.

## Installing Python Packages

If you choose to use `virtualenvwrapper` as recommended in the [build
instructions](build.html), it's not quite as automated in OS X as it is in most
Linux distributions. You can installed the necessary pieces this way:

```
sudo port install py27-pip py27-virtualenvwrapper
sudo port select --set pip pip27
sudo port select --set python python27
sudo port select --set virtualenv virtualenv27
```

Then add this line to your `~/.profile`:

```
. /opt/local/bin/virtualenvwrapper.sh-2.7
```

Finally, close any terminals you have open. When you next open a terminal you
will have the `virtualenvwrapper` commands (`mkvirtualenv`, `workon`, etc.)
available and can complete the build instructions.

## Finding Serial Devices

Whether you're using a USB FTDI serial adapter or a 3DR Radio, the path to that
device will be `/dev/cu.usbserial-<SERIAL>`, where `<SERIAL>` is the
manufacturer-assigned serial number. Things to note:

1.  If you have several serial adapters or radios, each one will have a
    different device path. That's important to be aware of if you're trying to
    write scripts to communicate with your quadcopter.

2.  In OS X, there is a second device node named `/dev/tty.usbserial-<SERIAL>`.
    **Do not use that device with this hardware**; it only works if the DCD line
    on the serial port is asserted, which it is not on these boards.

    If you use the wrong device, the symptom will be that mavproxy/gcs can't
    communicate with your vehicle.
