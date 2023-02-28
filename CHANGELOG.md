# h-raylib changelog

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
