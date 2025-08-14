{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TupleSections #-}

-- | Template Haskell functions used internally
module Raylib.Internal.TH (genNative) where

#ifdef WEB_FFI

import Language.Haskell.TH
  ( Body (NormalB),
    Clause (Clause),
    Dec (FunD, SigD),
    DecsQ,
    Exp (AppE, LitE, VarE),
    Lit (StringL),
    TypeQ,
    mkName,
  )
import Raylib.Internal.Web.Native (callRaylibFunction)

#else

import Language.Haskell.TH
  ( Dec (ForeignD),
    DecsQ,
    TypeQ,
    mkName, Foreign (ImportF), Callconv (CCall), Safety (Safe),
  )

#endif

-- | Generates native code for the given functions. On non-web platforms, this
--   means @foreign import@ statements. On web platforms, this means
--   `callRaylibFunction` calls.
genNative ::
  -- | (@hsName@, @cName@, @cFile@, @funType@)
  [(String, String, String, TypeQ)] ->
  DecsQ
genNative funs = do
  funs' <- mapM (\(a, b, c, d) -> (a,b,c,) <$> d) funs
  return (genNative' funs')
  where
    genNative' [] = []
#ifdef WEB_FFI
    genNative' ((hsName, cName, _, funType) : xs) =
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
    genNative' ((hsName, cName, cFile, funType) : xs) =
      -- foreign import ccall safe/unsafe "cFile cName" hsName :: funType
      ForeignD (ImportF CCall Safe (cFile ++ " " ++ cName) (mkName hsName) funType) : genNative' xs
#endif
