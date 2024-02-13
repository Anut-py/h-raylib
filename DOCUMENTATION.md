# h-raylib documentation

This file only contains h-raylib specific information. For documentation on individual functions, check the [raylib cheatsheet](https://www.raylib.com/cheatsheet/cheatsheet.html). You can also look at the examples included here in the `examples` directory (written in Haskell), or you can look at the more extensive set of examples [on the raylib website](https://www.raylib.com/examples.html) (written in C). For information about raylib in general, view the [raylib wiki](https://github.com/raysan5/raylib/wiki).

## Project structure

h-raylib contains bindings for raylib.h, raymath.h, rcamera.h, rlgl.h, and raygui.h.

TODO: move this documentation into Haddock

### Public modules

- `Raylib.Types`, `Raylib.Types.Core.*`, `Raylib.Types.Util.*`: provide all of raylib's types and implement low-level code to convert them to and from raw bytes (i.e. from C to Haskell and vice versa)
- `Raylib.Util`: provides miscellaneous utility functions
- `Raylib.Util.Colors`: provides some colors defined by raylib
- `Raylib.Util.GUI.Styles`: provides some styles defined by raygui
- `Raylib.Util.Lenses`: provides lenses for the raylib types
- `Raylib.Util.Camera`: Binds `rcamera.h` in pure Haskell
- `Raylib.Util.Lenses`: Binds `raymath.h` in pure Haskell
- `Raylib.Core`, `Raylib.Core.\*`, `Raylib.Util.\*`: correspond to their respective raylib modules

The binding functions in h-raylib are an almost one-to-one mapping to their corresponding raylib functions. The types and functions are, in some cases, slightly modified if it is possible to utilize Haskell features.

Below are some descriptions of these public modules and their purposes.

#### Raylib.Types.*.*

Each `Types` module has up to 3 sections: one for enumerations, one for structures, and one for callbacks. Any one of these may be omitted if not needed (for example, most of the `Types` modules do not have a callback section).

The enumerations section contains Haskell sum types that are instances of `Enum`. Each of these types corresponds to a raylib (C) `enum` or set of `define`s. The `fromEnum` and `toEnum` functions for these types use the numbers associated with these values in the C `enum`s. Most of these types are instances of `Storable` so they can be converted to raw bytes and passed to a C function. _NOTE: Some of these Haskell types correspond to C `enum`s that are in C source files, rather than header files._

The structures section contains Haskell types that correspond to each of raylib's `structs`. Each field in these types is named `typeName'fieldName` (e.g. the C struct `Vector2`'s `x` field is called `vector2'x` in Haskell). These structs also all derive the typeclass `Freeable` (declared in the internal `Raylib.Internal.Foreign` module). This typeclass allows types to describe how to properly free all the data associated with a pointer to that type. For example, `Image`'s implementation of `Freeable` also frees the pointer stored in the `Image.data` field in C. Finally, all of these types derive `Storable`, obviously, to convert them to and from pointers.

The callbacks section contains `FunPtr` types, along with higher-level Haskell wrappers. When you pass one of these wrappers (e.g. `LoadFileDataCallback`) to a function that takes one as an argument (e.g. `setLoadFileDataCallback`), the function will return a `FunPtr` type (e.g. `C'LoadFileDataCallback`). You will have to manually free this with `freeHaskellFunPtr` at the end of your program to avoid memory leaks (TODO: implement automatic memory management for `FunPtr`s).

`Raylib.Types` re-exports all of these types for convenience.

#### Raylib.Util

`Raylib.Util` contains some functions that may be useful for an h-raylib application. These functions are Haskell-only; i.e. they are not connected to C in any way. Additionally, it contains the Template Haskell function `raylibApplication`, which makes it easier to create a raylib application that supports native and web targets (TODO: add more details).

#### Raylib.Util.Colors

`Raylib.Util.Colors` is very simple: it declares 26 colors defined in `raylib.h`, namely `lightGray`, `gray`, `darkGray`, `yellow`, `gold`, `orange`, `pink`, `red`, `maroon`, `green`, `lime`, `darkGreen`, `skyBlue`, `blue`, `darkBlue`, `purple`, `violet`, `darkPurple`, `beige`, `brown`, `darkBrown`, `white`, `black`, `blank`, `magenta`, and `rayWhite`.

#### Raylib.Util.GUI.Styles

`Raylib.Util.GUI.Styles` binds predefined styles that are in `raygui/styles`. Calling one of these functions will activate that style. To return to the default style, simply call `guiLoadStyleDefault`.

#### Raylib.Util.Camera, Raylib.Util.Math

These modules contain pure functions (no `IO` monad) and data types. They are Haskell implementations of `rcamera` and `raymath`. They may not be an exact one-to-one mapping to the C code; some of the functions and high-level structures have been changed to be more idiomatic Haskell.

#### Raylib.Util.Lenses

Contains `lens` definitions for all the Haskell types.

#### Raylib.Core, Raylib.Core.\*, Raylib.Util.\*

These modules contain only functions. Each of these functions corresponds to a C function.

The `initWindow` function (Raylib.Core) returns a `WindowResources` value that must be passed to some `load*` functions and several other functions. 

The `unload*` functions are optional; even if you do not call them, all assets used by a program will be automatically be unloaded when it terminates. For some types (e.g. `Image`), an unloading function is not necessary.

These changes are for automatic memory management. See the "Memory management" section for details.

(Most) functions that took a pointer as an argument in C were changed to take a regular type as an argument and return an updated version of the argument (there are a few exceptions, because `Ptr`s are difficult to avoid in some cases).

##### Raylib.Util.GUI

This is a binding module for [raygui](https://github.com/raysan5/raygui), an immediate-mode GUI library built on top of raylib. The C version of raygui involves a lot of pointers because of the way it is designed. Unfortunately, this is problematic when binding it to Haskell, as Haskell's immutability makes it difficult to represent pointers properly. This means many functions will take the previous state of a control as an argument, and return the updated state of that control. You should use the Haddock documentation as a reference to make your life easier.

Keep in mind that raygui is an immediate mode GUI, so it is designed mostly for debugging and development and not for actual game GUIs. To this end, it is not very customizable and the features are quite limited. For a real game, you should make your own retained mode GUI.

### Private modules

#### Raylib.Internal

`Raylib.Internal` contains some functions used for automatic memory management. The automatic memory management flow is summarized in the "Memory management" section.

#### Raylib.Internal.Native

`Raylib.Internal.Native` consists solely of `foreign import` functions. These are used in the 6 public modules mentioned above. When compiling for the web, the `foreign import`s are replaced with `callRaylibFunction` calls (see the documentation for `Raylib.Internal.Web.Native` for details).

#### Raylib.Internal.Foreign

`Raylib.Internal.Foreign` contains miscellaneous utility functions for marshalling values to/from C. The most notable thing in this module is the `Freeable` typeclass.

The `Freeable` typeclass contains two methods, `rlFreeDependents` and `rlFree`. `rlFree` receives a pointer and frees all of the data associated with it, including the pointer itself. `rlFreeDependents` only frees the data "dependent" on the pointer, which usually means dynamic C arrays, i.e. pointers.

#### Raylib.Internal.Web.Native, Raylib.Internal.Web.Processable

_NOTE: These modules are only used when building for the web._

`Raylib.Internal.Web.Native` exports `callRaylibFunction`. This is an interfacing function that allows Haskell to call raylib functions that have been compiled with emscripten. This has to be done in a roundabout way because we cannot directly call these functions through Haskell; we have to call a JS function that calls the actual raylib functions.

`Raylib.Internal.Web.Processable` contains internal code to convert Haskell types to raw bytes.

## Memory management

The automatic memory management flow is as follows:

1. A `WindowResources` value is retrieved by calling `initWindow`.
2. A `load*` function is called (e.g. `loadModel`).
3. The data is loaded.
4. Any data that requires extra functions be called to unload it is stored in `Raylib.Internal` (e.g. shaders need to be freed from the GPU).
5. The window is closed and `closeWindow` is called.
6. All the data stored in `Raylib.Internal` is now unloaded (e.g. `rlUnloadShaderProgram` is called on all loaded shaders).

Keep in mind that this is all automatic; no extra action in the code is necessary for this to happen. Take a look at `Raylib.Internal` to see the functions used for this.

In some cases, models and other data used by a program are extremely large and thus expensive to keep in memory for the entire duration of the program. For this reason, the `unload*` functions are available to manually unload data on the fly.
