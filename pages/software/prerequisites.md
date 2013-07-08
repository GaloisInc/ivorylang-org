# Prerequisite Software

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

[Cabal][cabal] is the package manager for Haskell.

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

Finally, install the tool [cabal-dev][cabal-dev], which allows you to install packages to
a local sandbox:

```
cabal install cabal-dev
```

[cabal]: http://haskell.org/cabal
[cabal-download]: http://haskell.org/cabal/download.html
[smaccmpilot-build]: http://github.com/galoisinc/smaccmpilot-build
[cabal-dev]: http://hackage.haskell.org/package/cabal-dev

------------------------
### Continue to [building SMACCMPilot](build.html).
