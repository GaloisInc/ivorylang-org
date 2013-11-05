# Prerequisite Software

We have tested the software on various versions of Linux and make use of GNU
Make and a small Bash script.  For best results, ensure your system is
compatible (if you do not have Linux natively installed, running Linux in a
[virtual machine][vbox] is ideal).

[vbox]: https://www.virtualbox.org/

## GCC Cross Compiler

Building the SMACCMPilot C sources require a GCC toolchain for the ARM Cortex-M4
processor. We recommend using the *arm-gcc-embedded* toolchain, maintained by
ARM Ltd. [Download arm-gcc-embedded from Launchpad][arm-gcc-embedded].

You may install this toolchain into your PATH, or provide the installation
directory explicitly to the SMACCMPilot build.

[arm-gcc-embedded]: http://launchpad.net/gcc-arm-embedded

## The GHC Haskell Compiler

To build programs which use Ivory, you'll need the [Glasgow Haskell Compiler
(GHC)][ghc] version 7.6.2 or higher. You can either [download and
install][ghc762] the appropriate GHC binary release for your platform, or use
the latest release of the [Haskell Platform][haskell-platform], which provides
GHC 7.6.3.

[ghc]: http://www.haskell.org/ghc/
[ghc762]: http://www.haskell.org/ghc/download_ghc_7_6_2
[haskell-platform]: http://www.haskell.org/platform/

You can confirm the correct version of GHC is installed in your path by running

```
ghc --version
```

which should report version 7.6.2 or higher.

## Haskell Package Manager

[Cabal][cabal] is the package manager for Haskell. We require version 1.18 or
higher.

If you already installed the [Haskell Platform][haskell-platform], you already
have cabal. Otherwise, you'll need to install cabal separately. Cabal is
[downloadable as a binary][cabal-download].

Once you've installed cabal, you should update the package list on your machine:

```
cabal update
```

Then, upgrade your installation of cabal to the latest available on Hackage:

```
cabal install cabal-install
```

Check the version installed is 1.18 or greater:

```
cabal --version
```

[cabal]: http://haskell.org/cabal
[cabal-download]: http://haskell.org/cabal/download.html
[smaccmpilot-build]: http://github.com/galoisinc/smaccmpilot-build
[cabal-dev]: http://hackage.haskell.org/package/cabal-dev

------------------------

# Optional

## GCC Native Compiler

You need a native 32 bit GCC compiler of version 4.7.x, and the GCC plugin
development headers, to build applications which use our experimental runtime
verification (RTV) system. (The SMACCMPilot flight application does not use the
runtime verification system, so you may be able to regard this step as
optional.)

If you don't wish to use the runtime verification system, you can ignore
building the runtime verificaiton modules by running `make RTV=0` in
`smaccmpilot-build`, and disable building any apps that depend on RTV by
out the `CONFIG_BUILD_RTV` declaration in `smaccmpilot-stm32f4/Config.mk`.

The smaccmpilot-build makefile will use your native compiler to build a plugin
to be used by the gcc-arm-embedded cross compiler. Because the GCC plugin API
has changed several times in recent GCC releases, a GCC from the 4.7 series is
required. GCC 4.6 and 4.8 will not work. Additionally, because the
gcc-arm-embedded toolchain is built as 32-bit binaries, you will need a 32 bit
GCC to build a compatible plugin.

Your Linux package manager may package the GCC plugin development headers
separately from the gcc compiler. Fedora users will want to install the
`gcc-plugin-devel{.i686}` package; Ubuntu users will want to install
`gcc-4.7-plugin-dev`.

See the [runtime verification README][rv-readme] for more details.

[rv-app]: http://github.com/GaloisInc/smaccmpilot-stm32f4/tree/master/apps/sample-rtv-task
[rv-readme]: http://github.com/GaloisInc/ivory-rtverification/blob/master/README.md

-----
### Continue to [building SMACCMPilot](build.html).
