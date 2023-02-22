# h-raylib contributor guide

Anybody willing to contribute to the project is welcome to do so. Please use the GitHub issue tracker to report any bugs you find.

You can run the examples by using `cabal run {example name}` in the project directory.

You can use `run-all-examples.sh` to run all of the examples in one go.

[ROADMAP.md](https://github.com/Anut-py/h-raylib/blob/master/ROADMAP.md) is a list of features that may be added to this project. Contributors are welcome to help implement these.

## Project structure

_This section only contains h-raylib specific information. For information about raylib in general, view the [raylib wiki](https://github.com/raysan5/raylib/wiki)._

This project is split into 8 public modules. `Raylib.Types` contains all of raylib's types and low-level code to convert them to and from raw bytes. `Raylib.Colors` contains some colors defined by raylib. The other 6 public modules, `Core`, `Shapes`, `Textures`, `Text`, `Models`, and `Audio`, correspond to their respective raylib modules.

The functions in h-raylib are an almost one-to-one mapping to their corresponding raylib functions. The types are, in some cases, slightly modified if it is possible to utilize Haskell features.

Below are some descriptions of these public modules and their purposes.

### Raylib.Types

`Raylib.Types` has 4 sections: one for enumerations, one for typeclasses, one for structures, and one for callbacks.

The enumerations section contains Haskell sum types that are instances of `Enum`. Each of these types corresponds to a raylib `enum`. The `fromEnum` and `toEnum` functions for these types use the numbers associated with these values in the C `enum`s. Most of these types are instances of `Storable` so they can be converted to raw bytes and passed to a C function. _NOTE: Some of these Haskell types correspond to C `enum`s that are in C source files, rather than `raylib.h`._

The typeclasses section contains Haskell typeclasses that are derived by some of the types in the structures section.

The structures section contains Haskell types that correspond to each of raylib's `structs`. Each field in these types is named `typeName'fieldName` (e.g. the C struct `Vector2`'s `x` field is called `vector2'x` in Haskell). These structs also all derive the typeclass `Freeable` (declared in the internal `Raylib.Util` module). This typeclass allows types to describe how to properly free all the data associated with a pointer to that type. For example, `Image`'s implementation of `Freeable` also frees the pointer stored in the `Image.data` field in C. Finally, all of these types derive `Storable`, obviously, to convert them to and from pointers.

The callbacks section contains `FunPtr` types that are passed to some functions. _NOTE: These callbacks are very unlikely to be used, so they may be removed in the future._

### Raylib.Colors

`Raylib.Colors` is very simple: it declares 26 colors defined in `raylib.h`, namely `lightGray`, `gray`, `darkGray`, `yellow`, `gold`, `orange`, `pink`, `red`, `maroon`, `green`, `lime`, `darkGreen`, `skyBlue`, `blue`, `darkBlue`, `purple`, `violet`, `darkPurple`, `beige`, `brown`, `darkBrown`, `white`, `black`, `blank`, `magenta`, and `rayWhite`.

### The other 6 modules

These modules contain only functions. Each of these functions corresponds to a C function. The `unload*` functions were removed to make memory management automatic (this may be revised in the future, see `ROADMAP.md`). Functions that took a pointer as an argument in C were changed to take a regular type as an argument and return an updated version of the argument.

### Private modules

h-raylib has 3 modules that are not exposed for external use: `Raylib.Native`, `Raylib.Internal`, and `Raylib.Util`.

#### Raylib.Native

`Raylib.Native` consists solely of `foreign import` functions. These are used in the 6 public modules mentioned above.

#### Raylib.Internal

`Raylib.Internal` contains some functions used for automatic memory management. The automatic memory management flow is summarized in the "Memory management" section.

#### Raylib.Util

`Raylib.Util` contains miscellaneous utility functions for marshalling values to/from C. The most notable thing in this module is the `Freeable` typeclass.

The `Freeable` typeclass contains two methods, `rlFreeDependents` and `rlFree`. `rlFree` receives a pointer and frees all of the data associated with it, including the pointer itself. `rlFreeDependents` only frees the data "dependent" on the pointer, which usually means dynamic C arrays, i.e. pointers.

## Memory management

The automatic memory management flow is as follows:

1. A `load*` function is called (e.g. `loadModel`).
2. The data is loaded.
3. Any data that requires extra functions be called to unload it is stored in `Raylib.Internal` (e.g. shaders need to be freed from the GPU).
4. The window is closed and `closeWindow` is called.
5. All the data stored in `Raylib.Internal` is now unloaded (e.g. `rlUnloadShaderProgram` is called on all loaded shaders).

Keep in mind that this is all automatic; no extra action in the code is necessary for this to happen. Take a look at `Raylib.Internal` to see the functions used for this.

Unfortunately, this could lead to performance problems in larger projects, as large assets such as models must stay in memory for the duration of the program. This is why manual unloading is in the roadmap.
