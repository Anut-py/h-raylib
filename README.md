# h-raylib: Haskell bindings for Raylib

[![cabal-build](https://github.com/Anut-py/h-raylib/actions/workflows/cabal-build.yml/badge.svg)](https://github.com/Anut-py/h-raylib/actions/workflows/cabal-build.yml) [![nix-build](https://github.com/Anut-py/h-raylib/actions/workflows/nix-build.yml/badge.svg)](https://github.com/Anut-py/h-raylib/actions/workflows/nix-build.yml) [![Hackage Version](https://img.shields.io/hackage/v/h-raylib)](https://hackage.haskell.org/package/h-raylib)


This library includes Haskell bindings to the [Raylib](https://www.raylib.com/) library.

[Basic usage](#basic-usage) | [Platform specific requirements](#platform-specific-requirements) | [Advanced usage](#advanced-usage) | [GHCi](#running-in-ghci) | [Documentation](#documentation) | [FAQ and help](#faq-and-help)

## Basic usage

To use this package, include it as a dependency in your cabal file.

```cabal
build-depends:
  base,
  # ...
  h-raylib
```

It should work out of the box. See [Advanced usage](#advanced-usage) for more complex use cases.

## Platform specific requirements

This library is known to work on Windows, Linux, and Mac. It may not work properly on other platforms, so don't hesitate to report issues on the GitHub repository.

### Windows/Mac

h-raylib should automatically work if you do not disable the `detect-platform` flag. In that case, you may skip this step.

**If you do disable the `detect-platform` flag when building,** use the `platform-windows` or `platform-mac` flag when building.

### Linux

You may need to run the following to install [X11](https://en.wikipedia.org/wiki/X_Window_System) (a window manager for Linux).

```bash
sudo apt-get install libx11-dev libxrandr-dev libxi-dev libxcursor-dev libxinerama-dev
```

Now, h-raylib should automatically work if you do not disable the `detect-platform` flag. In that case, you may skip the following.

**If you do disable the `detect-platform` flag when building,** use the `platform-linux` flag when building

### BSD *(Experimental)*

h-raylib should automatically work if you do not disable the `detect-platform` flag. In that case, you may skip this step.

**If you do disable the `detect-platform` flag when building,** use the `platform-bsd` flag when building.

### Other platforms

This library has not yet been tested on other platforms (raylib supports
Android, Raspberry Pi, and DRM, all of which have not been implemented in
h-raylib). Anybody willing to try is welcome.

Web support has not been finalized yet.

If you get it working on other platforms, please create a pull request in the
GitHub repository and update `h-raylib.cabal` with the relevant config.

### Advanced usage

Your platform-specific dependencies will automatically be built by default. You
may want to disable this behavior. You can disable the `detect-platform` flag to
achieve this.

You can do this through the command line when running your project, like so

```sh
cabal run --constraint="h-raylib -detect-platform"
```

Or you can add it to your `cabal.project` file.

```
package h-raylib
  flags: -detect-platform
```

The flags `platform-windows`, `platform-mac`, `platform-linux`, and `platform-bsd` are also
supported if you want to build for a different platform.

If you prefer not to use the `lens` package, you may enable the `disable-lens` flag. This
removes the dependency on `lens` and disables `Raylib.Util.Lenses`.

## Running in GHCi

You can use this library in GHCi just like any other library, but you will need to add `--constraint="h-raylib +ghci"` to the command. For example, in the root folder of this repository, you could use the command below to use the library through GHCi.

```
cabal repl --constraint="h-raylib +ghci"
```

You may need to use `:set -fno-ghci-sandbox` after entering the REPL to fix problems with multithreaded execution.

On Windows, you may lose joystick support when running in GHCi.

## Documentation

For documentation: [DOCUMENTATION.md](https://github.com/Anut-py/h-raylib/blob/master/DOCUMENTATION.md).

For contributors: [CONTRIBUTING.md](https://github.com/Anut-py/h-raylib/blob/master/CONTRIBUTING.md).

If you want to request a feature, create an issue in the GitHub repo. Please check [the roadmap](https://github.com/Anut-py/h-raylib/blob/master/CONTRIBUTING.md#h-raylib-roadmap) to see if the feature has already been planned.

## FAQ and help

- When I try to run an h-raylib program I get the error `The code execution cannot proceed because libwinpthread-1.dll was not found. Reinstalling the program may fix this problem.`
  - See [#14](https://github.com/Anut-py/h-raylib/issues/14)
 
- When I try to compile an h-raylib program I get the error `Missing (or bad) C libraries: gcc_eh`
  - See [#36](https://github.com/Anut-py/h-raylib/issues/36)

If you find a bug, please [create an issue on GitHub](https://github.com/Anut-py/h-raylib/issues). There are probably some bindings that are incomplete or do not work properly, so if something seems wrong then it is most likely a bug.

If you have a question about the library that is not related to a bug, ask it on [GitHub discussions](https://github.com/Anut-py/h-raylib/discussions) or in the [Haskell GameDev Discord server](https://discord.gg/aKHNgxc59t).

## License

This project is licensed under the Apache License 2.0. See more in `LICENSE`.
