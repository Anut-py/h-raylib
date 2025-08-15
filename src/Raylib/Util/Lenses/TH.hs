module Raylib.Util.Lenses.TH (genLenses) where

import Language.Haskell.TH
  ( Con (RecC),
    Dec (DataD),
    DecsQ,
    Name,
    Info (TyConI),
    nameBase,
    reify
  )
import Control.Lens (makeLensesFor)
import Control.Monad (zipWithM)

-- | Creates lenses with an underscore before field names; e.g. @vector2'x@
--   becomes the lens @_vector2'x@
genLenses :: [Name] -> DecsQ
genLenses names = do
  infos <- mapM reify names
  concat <$> zipWithM genLensesForType names infos
  where
    genLensesForType name (TyConI (DataD _ _ _ _ [RecC _ ctors] _)) =
      makeLensesFor mapping name
      where
        mapping = map (\(a, _, _) -> let fName = nameBase a in (fName, '_' : fName)) ctors
    genLensesForType _ _ = error "(genLenses) Received a name that does not refer to a valid type!"
