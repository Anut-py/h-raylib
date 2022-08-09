# h-raylib: Haskell bindings for Raylib

This library includes low-level bindings to the [Raylib](https://www.raylib.com/) library from Haskell.

## Usage

To use this package, include it as a dependency in your cabal file.

```cabal
build-depends:
  base,
  # ...
  h-raylib
```

By default, h-raylib will automatically set up all the plaform specific requirements for your platform.

You can disable this behavior by adding the `no-autosetup` flag, e.g.

```cabal
build-depends:
  base,
  # ...
  h-raylib +no-autosetup
```

You may want to do this if you are using a platform that isn't supported by h-raylib autosetup.

## Platform specific requirements

This library has been tested on Windows and Ubuntu through WSL. It may not work properly on other platforms, so don't hesitate to report issues on the GitHub repository.

### Windows

h-raylib should automatically work if you do not pass the `no-autosetup` flag. In that case, you may skip this step.

**If you do add the `no-autosetup` flag when building:**

Add the following in your cabal file

```cabal
extra-libraries: gdi32 opengl32 winmm kernel32
```

### Ubuntu/Debian based systems

You may need to run the following command to install [X11](https://en.wikipedia.org/wiki/X_Window_System) (a window manager for Linux).

```bash
sudo apt-get install libxrandr-dev libxi-dev libxcursor-dev libxinerama-dev
```

Now, h-raylib should automatically work if you do not pass the `no-autosetup` flag. In that case, you may skip the following.

**If you do add the `no-autosetup` flag when building:**

Include the following in your cabal file

```cabal
extra-libraries: pthread m dl rt X11
```

### Other platforms

This library has not yet been tested on platforms other than Windows and Ubuntu. Anybody willing to try is welcome.

If you get it working on other platforms, please create a pull request in the
GitHub repository and update `h-raylib.cabal` with the relevant config.

## Contributing

Anybody willing to contribute to the project is welcome to do so. The project
source code is hosted on [GitHub](https://github.com/Anut-py/h-raylib). This
library is very new, so please report any bugs in the GitHub issue tracker.

You can run the example code by using `cabal run`.

## License

This project is licensed under the Apache License 2.0. See more in the `LICENSE` file.
