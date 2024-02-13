{-# OPTIONS -Wall #-}

{-|

Each @Types@ module has up to 3 sections: one for enumerations, one for
structures, and one for callbacks. Any one of these may be omitted if not
needed (for example, most of the @Types@ modules do not have a callback
section). Most of these types are instances of `Foreign.Storable` so they can
be converted to raw bytes and passed to a C function.

The enumerations section contains Haskell sum types that are instances of
@Enum@. Each of these types corresponds to a raylib (C) @enum@ or a set of
@define@ directives. The `Prelude.fromEnum` and `Prelude.toEnum` functions for
these types use the numbers associated with these values in C. /NOTE: Some of/
/these types correspond to C @enum@s that are defined in C source files, rather/
/than header files./

The structures section contains Haskell types that each correspond to a raylib
@struct@. Each field in these types is named @typeName'fieldName@ (e.g.
@Vector2.x@ in C is `vector2'x` in Haskell). These structs also all derive the
typeclass t`Raylib.Util.Freeable`. This typeclass allows types to describe how
to properly free all the data associated with a pointer to that type. For
example, `Image`'s implementation of @Freeable@ also frees the pointer stored
in the @Image.data@ field in C.

The callbacks section contains `Foreign.FunPtr` types, along with higher-level
Haskell wrappers. When you pass one of these wrappers (e.g.
`LoadFileDataCallback`) to a function that takes one as an argument (e.g.
`Raylib.Core.setLoadFileDataCallback`), the function will return a @FunPtr@
type (e.g. `C'LoadFileDataCallback`). You will have to manually free this with
`Foreign.freeHaskellFunPtr` at the end of your program to avoid memory leaks
(TODO: implement automatic memory management for `FunPtr`s).

-}

module Raylib.Types
  ( module Raylib.Types.Core,
    module Raylib.Types.Core.Audio,
    module Raylib.Types.Core.Camera,
    module Raylib.Types.Core.Models,
    module Raylib.Types.Core.Text,
    module Raylib.Types.Core.Textures,
    module Raylib.Types.Util.GUI,
    module Raylib.Types.Util.RLGL,
  ) where

import Raylib.Types.Core
import Raylib.Types.Core.Audio
import Raylib.Types.Core.Camera
import Raylib.Types.Core.Models
import Raylib.Types.Core.Text
import Raylib.Types.Core.Textures
import Raylib.Types.Util.GUI
import Raylib.Types.Util.RLGL
