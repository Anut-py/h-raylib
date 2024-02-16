# h-raylib roadmap

## Pending

Items which have not yet been worked on. Feel free to work on one of these.

- Implement automatic memory management for callbacks
- Bind `rgestures`

## In progress

- Add web build support \[[#4](https://github.com/Anut-py/h-raylib/issues/4)\]
- Move Raylib.Internal.Native functions into the modules where they are called
  - Use Template Haskell to clean up boilerplate

## Implemented

Items which have been completed but not published to hackage.

- Split Raylib.Types into multiple modules
- Bind `raygui` \[[#34](https://github.com/Anut-py/h-raylib/issues/34)\]

## Published

Items which have been published to hackage.

- Bind `raymath` (`4.6.0.1`)
- Bind `rcamera` (`4.6.0.1`)
- Bind `rlgl` (`4.5.3.2`)
- Allow manual unloading of assets for larger projects (`4.5.3.1`)
- Make it easier to pass shader parameters (`4.5.3.0`)
