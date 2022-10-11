# h-raylib: Haskell bindings for Raylib

This library includes Haskell bindings to the [Raylib](https://www.raylib.com/) library.

## Usage

To use this package, include it as a dependency in your cabal file.

```cabal
build-depends:
  base,
  # ...
  h-raylib
```

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

## Platform specific requirements

This library has been tested on Windows and Ubuntu through WSL. It may not work properly on other platforms, so don't hesitate to report issues on the GitHub repository.

### Windows

h-raylib should automatically work if you do not disable the `detect-platform` flag. In that case, you may skip this step.

**If you do disable the `detect-platform` flag when building:**

Add the following in your cabal file, or use the `platform-windows` flag when building.

```cabal
extra-libraries: gdi32 opengl32 winmm kernel32
```

### Ubuntu/Debian based systems

You may need to run the following to install [X11](https://en.wikipedia.org/wiki/X_Window_System) (a window manager for Linux).

```bash
sudo apt-get install libxrandr-dev libxi-dev libxcursor-dev libxinerama-dev
```

Now, h-raylib should automatically work if you do not disable the `detect-platform` flag. In that case, you may skip the following.

**If you do disable the `detect-platform` flag when building:**

Include the following in your cabal file, or use the `platform-linux` flag when building

```cabal
extra-libraries: GL pthread m dl rt X11
```

### Other platforms

This library has not yet been tested on platforms other than Windows and Ubuntu. Anybody willing to try is welcome.

If you get it working on other platforms, please create a pull request in the
GitHub repository and update `h-raylib.cabal` with the relevant config.

## Contributing

Anybody willing to contribute to the project is welcome to do so. This
library is very new, so please report any bugs in the GitHub issue tracker.

You can run the examples by using `cabal run example-project-name`.

## License

This project is licensed under the Apache License 2.0. See more in `LICENSE`.
