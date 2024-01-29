# h-raylib documentation

This file only contains h-raylib specific information. For documentation on individual functions, check the [raylib cheatsheet](https://www.raylib.com/cheatsheet/cheatsheet.html). You can also look at the examples included here in the `examples` directory (written in Haskell), or you can look at the more extensive set of examples [on the raylib website](https://www.raylib.com/examples.html) (written in C). For information about raylib in general, view the [raylib wiki](https://github.com/raysan5/raylib/wiki).

## Project structure

### Public modules

`Raylib.Types` contains all of raylib's types and low-level code to convert them to and from raw bytes (i.e. from C to Haskell and vice versa). `Raylib.Util` contains miscellaneous utility functions. `Raylib.Util.Colors` contains some colors defined by raylib. `Raylib.Util.Lenses` provides lenses for the raylib types. The other public modules, `Raylib.Core`, `Raylib.Core.\*`, and `Raylib.Util.\*`, correspond to their respective raylib modules.

The binding functions in h-raylib are an almost one-to-one mapping to their corresponding raylib functions. The types and functions are, in some cases, slightly modified if it is possible to utilize Haskell features.

Below are some descriptions of these public modules and their purposes.

#### Raylib.Types

`Raylib.Types` has 3 sections: one for enumerations, one for structures, and one for callbacks.

The enumerations section contains Haskell sum types that are instances of `Enum`. Each of these types corresponds to a raylib `enum` or set of `define`s. The `fromEnum` and `toEnum` functions for these types use the numbers associated with these values in the C `enum`s. Most of these types are instances of `Storable` so they can be converted to raw bytes and passed to a C function. _NOTE: Some of these Haskell types correspond to C `enum`s that are in C source files, rather than header files._

The structures section contains Haskell types that correspond to each of raylib's `structs`. Each field in these types is named `typeName'fieldName` (e.g. the C struct `Vector2`'s `x` field is called `vector2'x` in Haskell). These structs also all derive the typeclass `Freeable` (declared in the internal `Raylib.ForeignUtil` module). This typeclass allows types to describe how to properly free all the data associated with a pointer to that type. For example, `Image`'s implementation of `Freeable` also frees the pointer stored in the `Image.data` field in C. Finally, all of these types derive `Storable`, obviously, to convert them to and from pointers.

The callbacks section contains `FunPtr` types that are passed to some functions.

#### Raylib.Util

`Raylib.Util` contains some functions that may be useful for an h-raylib application. These functions are Haskell-only; i.e. they are not connected to C in any way.

#### Raylib.Util.Colors

`Raylib.Util.Colors` is very simple: it declares 26 colors defined in `raylib.h`, namely `lightGray`, `gray`, `darkGray`, `yellow`, `gold`, `orange`, `pink`, `red`, `maroon`, `green`, `lime`, `darkGreen`, `skyBlue`, `blue`, `darkBlue`, `purple`, `violet`, `darkPurple`, `beige`, `brown`, `darkBrown`, `white`, `black`, `blank`, `magenta`, and `rayWhite`.

#### Raylib.Util.Camera, Raylib.Util.Math

These modules contain pure functions (no `IO` monad) and data types. They are Haskell implementations of `rcamera` and `raymath`. They may not be an exact one-to-one mapping to the C code; some of the functions and high-level structures have been changed to be more idiomatic Haskell.

#### Raylib.Util.Lenses

Contains `lens` definitions for all the Haskell types.

#### Raylib.Core, Raylib.Core.\*, Raylib.Util.\*

These modules contain only functions. Each of these functions corresponds to a C function.

The `initWindow` function returns a `WindowResources` value that must be passed to some `load*` functions and several other functions. 

The `unload*` functions are optional; even if you do not call them, all assets used by a program will be automatically be unloaded when it terminates. For some types (e.g. `Image`), an unloading function is not necessary.

These changes are for automatic memory management. See the "Memory management" section for details.

Functions that took a pointer as an argument in C were changed to take a regular type as an argument and return an updated version of the argument.

### Private modules

h-raylib has 3 modules that are not exposed for external use: `Raylib.Native`, `Raylib.Internal`, and `Raylib.ForeignUtil`.

#### Raylib.Native

`Raylib.Native` consists solely of `foreign import` functions. These are used in the 6 public modules mentioned above. (TODO: web support)

#### Raylib.Internal

`Raylib.Internal` contains some functions used for automatic memory management. The automatic memory management flow is summarized in the "Memory management" section.

#### Raylib.ForeignUtil

`Raylib.ForeignUtil` contains miscellaneous utility functions for marshalling values to/from C. The most notable thing in this module is the `Freeable` typeclass.

The `Freeable` typeclass contains two methods, `rlFreeDependents` and `rlFree`. `rlFree` receives a pointer and frees all of the data associated with it, including the pointer itself. `rlFreeDependents` only frees the data "dependent" on the pointer, which usually means dynamic C arrays, i.e. pointers.

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
