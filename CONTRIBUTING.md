# h-raylib contributor guide

Anybody willing to contribute to the project is welcome to do so. Please use the GitHub issue tracker to report any bugs you find.

You can run the examples by using `cabal run [example-name]` in the project directory.

You can run an example with profiling by using `cabal run [example-name] --ghc-options="-fprof-auto -rtsopts -threaded" --enable-library-profiling --enable-profiling -- +RTS -N -P`. This will generate a file `example-name.prof` with the profiling info.

You can use `run-all-examples.sh` to run all of the examples in one go.

You can use `./devtools.js [options]` (on Linux) or `node devtools.js [options]` anywhere else for general development utilities. Use the `--help` flag for details.

[DOCUMENTATION.md](https://github.com/Anut-py/h-raylib/blob/master/DOCUMENTATION.md) has information that is specific to h-raylib.

## h-raylib roadmap

This is a list of features that may be added to this project. Contributors are welcome to help implement these.

### Pending

Items which have not yet been worked on. Feel free to work on one of these.

- Bind `rgestures`

### In progress

- Add web build support \[[#4](https://github.com/Anut-py/h-raylib/issues/4)\]

### Implemented

Items which have been completed but not published to hackage.

(none)

### Published

Items which have been published to hackage.

- Implement automatic memory management for callbacks (`5.1.1.0`)
- Move Raylib.Internal.Native functions into the modules where they are called (`5.1.1.0`)
  - Use Template Haskell to clean up boilerplate
- Split Raylib.Types into multiple modules (`5.1.1.0`)
- Bind `raygui` \[[#34](https://github.com/Anut-py/h-raylib/issues/34)\]  (`5.1.1.0`)
- Bind `raymath` (`4.6.0.1`)
- Bind `rcamera` (`4.6.0.1`)
- Bind `rlgl` (`4.5.3.2`)
- Allow manual unloading of assets for larger projects (`4.5.3.1`)
- Make it easier to pass shader parameters (`4.5.3.0`)
