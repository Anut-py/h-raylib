# h-raylib changelog

## Version 4.5.0.4
_13 November 2022_
- Replaced `CInt` with `CBool` for functions that return booleans
- Removed `Xext` dependency (it is no longer required for Nix builds)

## Version 4.5.0.5
_19 November 2022_
- Replaced `CInt` with `CBool` in `RayCollision`
- Updated raylib to the master branch

## Version 4.5.0.6
_24 November 2022_

\[[#6](https://github.com/Anut-py/h-raylib/issues/6)\]

- Fixed `Font` marshalling

## Version 4.5.0.7
_26 November 2022_

\[[#7](https://github.com/Anut-py/h-raylib/pull/7)\]

- Removed all constants that were enums in the original C API and replaced them with sum types deriving Enum
- Removed some CInt usage in the main API
- Removed `Raylib.Constants`

## Version 4.5.0.8
_18 December 2022_

\[[#9](https://github.com/Anut-py/h-raylib/issues/9)\]

- Fixed an issue on Mac where `clang` failed to detect that `rglfw.c` was using objective-c

## Version 4.5.0.9
_23 December 2022_

- Changed `setConfigFlags` and `setGesturesEnabled` to use an array of flags
