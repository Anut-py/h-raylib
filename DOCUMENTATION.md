# h-raylib documentation

This file only contains h-raylib specific information. For documentation on individual functions, check the [raylib cheatsheet](https://www.raylib.com/cheatsheet/cheatsheet.html) and the [h-raylib Haddock documentation](https://hackage.haskell.org/package/h-raylib). You can also look at the examples included here in the `examples` directory (written in Haskell), or you can look at the more extensive set of examples [on the raylib website](https://www.raylib.com/examples.html) (written in C). For information about raylib in general, view the [raylib wiki](https://github.com/raysan5/raylib/wiki).

For regular users of the library, the Haddock documentation should be more than enough (although much of it is auto-generated, so make sure to create an issue if you find anything that seems wrong). The documentation included here is more useful for contributors or anyone who wants to learn about how h-raylib works internally.

## Versioning scheme

The first two numbers in the version track the underlying C raylib version. For example, `5.1.x.x` versions use raylib 5.1 under the hood. The third number represents breaking changes (renamed/deleted functions or modules). The last number represents non-breaking changes (new functions or modules, bug fixes, etc).

## Project structure

h-raylib contains bindings for raylib.h, raymath.h, rcamera.h, rlgl.h, and raygui.h. The public modules' documentation can be found in Haddock, as mentioned above, and documentation for the private modules can be found in the source files.

### Public modules

The binding functions in h-raylib are an almost one-to-one mapping to their corresponding raylib functions. The types and functions are, in some cases, slightly modified if it is possible to utilize Haskell features.

The `initWindow` function (Raylib.Core) returns a `WindowResources` value that must be passed to some `load*` functions and several other functions. 

The `unload*` functions are optional; even if you do not call them, all assets used by a program will be automatically be unloaded when it terminates. For some types (e.g. `Image`), an unloading function is not necessary.

These changes are for automatic memory management. See the "Memory management" section for details.

(Most) functions that took a pointer as an argument in C were changed to take a regular type as an argument and return an updated version of the argument (there are a few exceptions, because `Ptr`s are difficult to avoid in some cases).

You can also access the underlying C functions by prepending `c'` to the function name (e.g. `c'initWindow`). This may be necessary for performance reasons. Note that if you do this, you will have to manually unload and free everything.

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
