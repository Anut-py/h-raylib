# h-raylib documentation

This file only contains h-raylib specific information. For documentation on individual functions, check the [raylib cheatsheet](https://www.raylib.com/cheatsheet/cheatsheet.html) and the [h-raylib Haddock documentation (incomplete)](https://hackage.haskell.org/package/h-raylib). You can also look at the examples included here in the `examples` directory (written in Haskell), or you can look at the more extensive set of examples [on the raylib website](https://www.raylib.com/examples.html) (written in C). For information about raylib in general, view the [raylib wiki](https://github.com/raysan5/raylib/wiki).

## Memory management

### Automatic management flow (recommended)

This flow is the one used in most of the example programs and should be used unless it is absolutely necessary not to.

1. A `WindowResources` handle is retrieved by calling `initWindow`
2. A `load*` function is called (e.g. `loadModel`) along with `managed` (e.g. `model <- managed window $ loadModel filePath`, where `window :: WindowResources`)
3. The data is loaded
4. Any data that requires extra functions be called to unload it is automatically stored in the `WindowResources` handle
5. The window is closed and `closeWindow` is called with the `WindowResources` handle (`closeWindow (Just window)`)
6. All the data stored in the handle is now automatically unloaded

In some cases, models and other data used by a program are extremely large and thus expensive to keep in memory for the entire duration of the program. For this reason, the `unload*` functions are available to manually unload auto-managed data on the fly (be sure *not* to use the `close` function on automatically managed resources).

Make sure to take a look at the examples to get a better understanding of this.

### Manual management flow

In some cases, you may want to opt out of automatic memory management, like so:

1. `initWindowUnmanaged` is called (it does not return a `WindowResources` handle)
2. A `load*` function is called *without* using `managed` (e.g. `model <- loadModel filePath`)
3. The data is loaded
4. The data must be manually unloaded with `close` (e.g. `close model`)
5. The window is closed by calling `closeWindow` without a `WindowResources` handle (`closeWindow Nothing`)

Note that if you are using the automatic memory management flow, then you can still load unmanaged resources by simply omitting `managed` when loading something (e.g. `model <- loadModel filePath`); however, you will still have to unload it manually with `close`, just like in the manual management flow.

The `Raylib.Internal` module is available for advanced users who want to create their own `WindowResources` object and manually add resources to it. This should be used with caution; it is error-prone, and in the vast majority of cases, simply using the automatic or manual management flows is sufficient.

## Project structure

h-raylib contains bindings for raylib.h, raymath.h, rcamera.h, rlgl.h, and raygui.h. The public modules' documentation can be found in Haddock, as mentioned above, and documentation for the private modules can be found in the source files.

### Public modules

The binding functions in h-raylib are an almost one-to-one mapping to their corresponding raylib functions. The types and functions are, in some cases, slightly modified if it is possible to utilize Haskell features.

The `initWindow` function (Raylib.Core) returns a `WindowResources` value that must be passed to the `managed` function for automatic memory management.

(Most) functions that took a pointer as an argument in C were changed to take a regular type as an argument and return an updated version of the argument (there are a few exceptions, because `Ptr`s are difficult to avoid in some cases).

You can also access the underlying C functions by prepending `c'` to the function name (e.g. `c'initWindow`). This may be necessary for performance reasons. Note that if you do this, you will have to manually unload and free everything.
