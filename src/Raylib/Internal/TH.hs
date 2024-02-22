{-# OPTIONS -Wall -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TupleSections #-}

-- | Template Haskell functions used internally
module Raylib.Internal.TH (genLenses, genNative) where

import Control.Lens (makeLensesFor)
import Control.Monad (zipWithM)

#ifdef WEB_FFI

import Language.Haskell.TH
  ( Body (NormalB),
    Clause (Clause),
    Con (RecC),
    Dec (DataD, FunD, SigD),
    DecsQ,
    Exp (AppE, LitE, VarE),
    Info (TyConI),
    Lit (StringL),
    Name,
    TypeQ,
    mkName,
    nameBase,
    reify,
  )
import Raylib.Internal.Web.Native (callRaylibFunction)

#else

import Language.Haskell.TH
  ( Con (RecC),
    Dec (DataD, ForeignD),
    DecsQ,
    Info (TyConI),
    Name,
    TypeQ,
    mkName,
    nameBase,
    reify, Foreign (ImportF), Callconv (CCall), Safety (Safe, Unsafe),
  )

#endif

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

-- | Generates native code for the given functions. On non-web platforms, this
--   means @foreign import@ statements. On web platforms, this means
--   `callRaylibFunction` calls.
genNative ::
  -- | (@hsName@, @cName@, @cFile@, @funType@, @isSafe@)
  [(String, String, String, TypeQ, Bool)] ->
  DecsQ
genNative funs = do
  funs' <- mapM (\(a, b, c, d, e) -> (a,b,c,,e) <$> d) funs
  return (genNative' funs')
  where
    genNative' [] = []
#ifdef WEB_FFI
    genNative' ((hsName, cName, _, funType, _) : xs) =
      [ -- hsName :: funType
        SigD name funType,
        -- hsName = callRaylibFunction "_cName"
        FunD
          name
          [Clause [] (NormalB (AppE (VarE 'callRaylibFunction) (LitE (StringL ('_' : cName))))) []]
      ] ++ genNative' xs
      where
        name = mkName hsName
#else
    genNative' ((hsName, cName, cFile, funType, isSafe) : xs) =
      -- foreign import ccall safe/unsafe "cFile cName" hsName :: funType
      ForeignD (ImportF CCall (if isSafe then Safe else Unsafe) (cFile ++ " " ++ cName) (mkName hsName) funType) : genNative' xs
#endif
