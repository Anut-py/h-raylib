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
- Fixed `Font` marshalling ([#6](https://github.com/Anut-py/h-raylib/issues/6))

## Version 4.5.0.7
_26 November 2022_
- Removed all constants that were enums in the original C API and replaced them with sum types deriving Enum. Also removed some CInt usage in the main API. ([#7](https://github.com/Anut-py/h-raylib/pull/7))
