# h-raylib changelog

## Version 5.1.1.0
_22 February, 2024_

- Split `Raylib.Types` into different modules (`Raylib.Types` reexports everything, so this will not break existing code)
- Fixed `flake.nix` and `default.nix`
- Exposed all the native functions
- Added support for callbacks
- Added more thorough Haddock documentation
- Added pointer utility functions (`p'*`)

\[[#4](https://github.com/Anut-py/h-raylib/issues/4)\]

- Started working on web support

\[[#34](https://github.com/Anut-py/h-raylib/issues/34)\]

- Added bindings for raygui (`Raylib.Util.GUI`, `Raylib.Util.GUI.Styles`)

## Version 5.1.0.1
_16 January, 2024_

- Updated raylib to the master branch

\[[#26](https://github.com/Anut-py/h-raylib/pull/26)\]

- New `platform-nixos` build flag

\[[#27](https://github.com/Anut-py/h-raylib/pull/27), [#35](https://github.com/Anut-py/h-raylib/pull/35)\]

- Added flake.nix

## Version 4.6.0.7
_10 September, 2023_

- Updated raylib to the master branch

## Version 4.6.0.6
_24 July, 2023_

- Updated raylib to the master branch

## Version 4.6.0.5
_29 June, 2023_

\[[#22](https://github.com/Anut-py/h-raylib/pull/22)\]

- Allowed `base-4.18` to support GHC 9.4 and higher

\[[#23](https://github.com/Anut-py/h-raylib/pull/23)\]

- Added helper functions in `Raylib.Util` as an alternative to the `begin*` and `end*` functions

## Version 4.6.0.4
_16 June, 2023_

- Updated raylib to the master branch

\[[#19](https://github.com/Anut-py/h-raylib/pull/19)\]

- Changed all the `Raylib.Util` functions to use `MonadIO` for flexibility

\[[#20](https://github.com/Anut-py/h-raylib/pull/20)\]

- Changed `CFloat` to `Float` in `drawCapsule` and `drawCapsuleWires`

## Version 4.6.0.3
_23 April, 2023_

- Updated raylib to the master branch

\[[#18](https://github.com/Anut-py/h-raylib/pull/18)\]

- Added lenses for raylib data structures

## Version 4.6.0.2
_8 April, 2023_

- Fixed a bug in `clamp`
- Updated raylib to the master branch

## Version 4.6.0.1
_2 April, 2023_

- Created the `Raylib.Util.Math` and `Raylib.Util.Camera` modules. They are Haskell implementations of `raymath` and `rcamera`.

\[[#15](https://github.com/Anut-py/h-raylib/pull/15)\]

- Fixed a memory issue with `getFontDefault`

## Version 4.5.3.4
_19 March, 2023_

- Updated raylib to the master branch

## Version 4.5.3.3
_15 March, 2023_

- Added GHCi support (see README.md for usage instructions)
- Updated raylib to the master branch
- Removed global state; use `WindowResources` (see examples for usage)

## Version 4.5.3.2
_1 March, 2023_

\[[#12](https://github.com/Anut-py/h-raylib/pull/12)\]

- Added rlgl bindings (`Raylib.Util.RLGL`)

## Version 4.5.3.1
_27 February, 2023_

- Added manual asset unloading functions
- Updated raylib to the master branch

\[[#11](https://github.com/Anut-py/h-raylib/pull/11)\]

- Fixed a build issue on MacOS

## Version 4.5.3.0
_24 February, 2023_

-  **BREAKING CHANGE**: Restructured project; the main modules are moved into `Raylib/Core` and `Raylib.Colors` is now `Raylib.Util.Colors`
- Changed `setShaderValue` and `setShaderValueV` to consume Haskell values rather than `Ptr`s
- Added the `Raylib.Util` module for utility functions

## Version 4.5.2.0
_21 February, 2023_

- **BREAKING CHANGE**: Removed asset unloading functions
- **BREAKING CHANGE**: Changed `Camera3D` API to match C code
- Added code to automatically unload assets
- Added mathematical operators for vector types
- Updated raylib to the master branch

## Version 4.5.1.1
_14 February, 2023_

- Added finalizers to auto-unload audio data
- Fixed C include errors

## Version 4.5.1.0
_12 February, 2023_

- **BREAKING CHANGE**: Changed all types to minimize usage of `Ptr`s
- **BREAKING CHANGE**: Split the `Raylib` module into six modules: `Raylib.Audio`, `Raylib.Core`, `Raylib.Models`, `Raylib.Shapes`, `Raylib.Text`, and `Raylib.Textures`
- Added the internal `Freeable` typeclass to prevent memory leaks

\[[#8](https://github.com/Anut-py/h-raylib/issues/8)\]

- Added `Xext` as a dependency again

## Version 4.5.0.12
_14 January, 2023_

- Removed `ShaderLocationIndex` from some function types

## Version 4.5.0.11
_14 January, 2023_

- Fixed some function types
- Allowed omitting fragment/vertex shaders in `loadShader` functions

## Version 4.5.0.10
_5 January, 2023_

- Restructured to make the examples easier to run
- Updated raylib to the master branch

## Version 4.5.0.9
_23 December, 2022_

- Changed `setConfigFlags` and `setGesturesEnabled` to use an array of flags

## Version 4.5.0.8
_18 December, 2022_

\[[#9](https://github.com/Anut-py/h-raylib/issues/9)\]

- Fixed an issue on Mac where `clang` failed to detect that `rglfw.c` was using objective-c

## Version 4.5.0.7
_26 November, 2022_

\[[#7](https://github.com/Anut-py/h-raylib/pull/7)\]

- Removed all constants that were enums in the original C API and replaced them with sum types deriving `Enum`
- Removed some `CInt` usage in the main API
- Removed `Raylib.Constants`

## Version 4.5.0.6
_24 November, 2022_

\[[#6](https://github.com/Anut-py/h-raylib/issues/6)\]

- Fixed `Font` marshalling

## Version 4.5.0.5
_19 November, 2022_
- Replaced `CInt` with `CBool` in `RayCollision`
- Updated raylib to the master branch

## Version 4.5.0.4
_13 November, 2022_
- Replaced `CInt` with `CBool` for functions that return booleans
- Removed `Xext` dependency (it is no longer required for Nix builds)
