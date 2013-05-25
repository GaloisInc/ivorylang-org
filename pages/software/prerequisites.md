# Prerequisite Software

## GCC Cross Compiler

Building the SMACCMPilot C sources require a GCC toolchain for the ARM Cortex-M4
processor. We recommend the *arm-gcc-embedded* toolchain [available on
Launchpad][arm-gcc-embedded].

You may install this toolchain into your PATH, or provide the installation
directory explicitly to the SMACCMPilot build.

[arm-gcc-embedded]: http://launchpad.net/gcc-arm-embedded

## The GHC Haskell Compiler

To build programs which use Ivory, you'll need the [Glasgow Haskell Compiler
(GHC)][ghc] version 7.6.2. (We've had problems with the latest release 7.6.3). Please
[download and install][ghc762] the appropriate GHC binary release for your
platform.

[ghc]: http://www.haskell.org/ghc/
[ghc762]: http://www.haskell.org/ghc/download_ghc_7_6_2

You can confirm the correct version of GHC is installed in your path by running

```
ghc --version
```

which should report version 7.6.2.

## Haskell Package Manager

[Cabal][cabal] is the package manager for Haskell. You'll need to install Cabal
([downloadable as a binary][cabal-download]) in order to install the
prerequisites for the [SMACCMPilot build][smaccmpilot-build].

Once you've installed cabal, you should update the package list on your machine:

```
cabal update
```

Then, upgrade your installation of cabal to the latest available on Hackage:

```
cabal install cabal-install
```

Finally, install the tool [cabal-dev][cabal-dev], which allows you to install packages to
a local sandbox:

```
cabal install cabal-dev
```

[cabal]: http://haskell.org/cabal
[smaccmpilot-build]: http://github.com/galoisinc/smaccmpilot-build
[cabal-dev]: http://hackage.haskell.org/package/cabal-dev

------------------------
### Continue to [building SMACCMPilot](build.html).
