{-# LANGUAGE CPP, TemplateHaskell, QuasiQuotes #-}

module GHCJS.VDOM.Internal.TH where

import Data.List (foldl')

import Language.Haskell.TH

import Unsafe.Coerce

mkTupleChildrenInstances :: Name -> Name -> Name -> Name -> Name -> [Int] -> Q [Dec]
mkTupleChildrenInstances cls method ty con wrapper xs =
  concat <$> mapM (mkTupleChildrenInstance cls method ty con wrapper) xs

{-

instance cls (ty, ty, ...) where
  method (con x1, con x2, ...) = wrapper (buildArrayIN x1 x2 ...)
  {-# INLINE method #-}

-}
mkTupleChildrenInstance :: Name -> Name -> Name -> Name -> Name -> Int -> Q [Dec]
mkTupleChildrenInstance cls method ty con wrapper n = do
  let xs    = map (mkName.('x':).show) [1..n]
      t     = AppT (ConT cls) (iterate (`AppT` ConT ty) (TupleT n) !! n)
      build = mkName ("GHCJS.Prim.Internal.Build.buildArrayI" ++ show n)
      pat   = [TupP (map (ConP con . (:[]) . VarP) xs)]
      body  = NormalB (AppE (ConE wrapper)
                           (foldl' (\e v -> AppE e (VarE v)) (VarE build) xs))
#if MIN_VERSION_template_haskell(2,11,0)
  return [InstanceD Nothing [] t [ FunD method [Clause pat body []]
                                 , PragmaD (InlineP method Inline FunLike AllPhases)
                                 ]
         ]
#else
  return [InstanceD [] t [ FunD method [Clause pat body []]
                         , PragmaD (InlineP method Inline FunLike AllPhases)
                         ]
         ]
#endif
mkTupleAttrInstances :: Name -> Name -> Name -> Name -> Name -> [Int] -> Q [Dec]
mkTupleAttrInstances cls method ty con wrapper xs =
  concat <$> mapM (mkTupleAttrInstance cls method ty con wrapper) xs

{-

instance cls (ty, ty, ...) where
  method (con k1 v1, con k2 v2, ...) =
    wrapper (buildObjectIN k1 k2 v1 v2 ...)
  {-# INLINE method #-}

 -}
mkTupleAttrInstance :: Name -> Name -> Name -> Name -> Name -> Int -> Q [Dec]
mkTupleAttrInstance cls method ty con wrapper n = do
  let xs    = map (\i -> let si = show i in [mkName ('k':si), mkName ('v':si)]) [1..n]
      t     = AppT (ConT cls) (iterate (`AppT` ConT ty) (TupleT n) !! n)
      build = mkName ("GHCJS.Prim.Internal.Build.buildObjectI" ++ show n)
      pat   = [TupP (map (ConP con . map VarP) xs)]
      app e k v = AppE (AppE e (AppE (VarE 'unsafeCoerce) (VarE k))) (VarE v)
      body  = NormalB (AppE (ConE wrapper)
                           (foldl' (\e [k,v] -> app e k v) (VarE build) xs))
#if MIN_VERSION_template_haskell(2,11,0)
  return [InstanceD Nothing [] t [ FunD method [Clause pat body []]
                                 , PragmaD (InlineP method Inline FunLike AllPhases)
                                 ]
         ]
#else
  return [InstanceD [] t [ FunD method [Clause pat body []]
                         , PragmaD (InlineP method Inline FunLike AllPhases)
                         ]
         ]
#endif
