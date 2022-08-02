# h-raylib: Haskell bindings for Raylib

This library includes low-level bindings to the [Raylib](https://www.raylib.com/) library from Haskell.

## Usage

To use this package, include it as a dependency in you cabal file.

```cabal
build-depends:
  base,
  # ...
  h-raylib
```

## Platform specific requirements

This library has been tested on Windows and Ubuntu through WSL. It may not work properly on other platforms, so don't hesitate to report issues on the GitHub repository.

### Windows

Add the following in your cabal file

```cabal
extra-libraries: gdi32 opengl32 winmm kernel32
```

This should work without any other setup.

### Ubuntu/Debian based systems

You may need to run the following command to install [X11](https://en.wikipedia.org/wiki/X_Window_System) (a window manager for Linux).

```sh
sudo apt-get install libxrandr-dev libxi-dev libxcursor-dev libxinerama-dev
```

Next, include the following in your cabal file

```cabal
extra-libraries: pthread m dl rt X11
```

This should work without any other setup.

Please refer to the examples in `h-raylib.cabal` in the source code for examples of this in use.

### Other platforms

This library has not yet been tested on platforms other than Windows and Ubuntu. Anybody willing to try is welcome.

If you get it working on other platforms, please create a PR and update `h-raylib.cabal` with the relevant config in the source code.

## Contributing

Anybody willing to contribute to the project is welcome to do so. The project source code is hosted on [GitHub](https://github.com/Anut-py/h-raylib). This library is very new, so please report any bugs in the GitHub issue tracker.

You can run the example code by using `cabal run`.

## License

This project is licensed under the Apache License 2.0. See more in the `LICENSE` file in the source code.
