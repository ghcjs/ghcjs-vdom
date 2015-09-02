{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GHCJS.VDOM.Internal where

import GHCJS.VDOM.Internal.Types

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

import GHC.Prim (Any, State#, RealWorld)

import Control.Monad
import Unsafe.Coerce

import GHCJS.Foreign.QQ
import GHCJS.Types
import GHCJS.Marshal.Pure

import Data.List (foldl')
import Data.String (IsString(..))
import Data.Typeable

import GHC.IO           ( IO(..) )
import GHC.Base         ( StableName# )

type J = JSRef

j :: QuasiQuoter
j = jsu'

mkVNode :: (Attributes a, Children c) => JSString -> a -> c -> VNode
mkVNode tag atts children = js_vnode tag (mkAttributes atts) (mkChildren children)
{-# INLINE mkVNode #-}

mkElems :: [String] -> Q [Dec]
mkElems = fmap concat . mapM (join mkElem)

mkElem :: String -> String -> Q [Dec]
mkElem name tag = do
     let n = mkName name
     a <- newName "a"
     c <- newName "c"
     b <- [| mkVNode (fromString tag) |]
     typ <- [t|forall a c. (Attributes a, Children c) => a -> c -> VNode |]
     return [ SigD n typ
            , FunD n [Clause [VarP a, VarP c] (NormalB (AppE (AppE b (VarE a)) (VarE c))) []]
            , PragmaD (InlineP n Inline FunLike AllPhases)
            ]

mkAttrs :: Name -> [String] -> Q [Dec]
mkAttrs ty = fmap concat . mapM (join (mkAttr ty))

mkAttrs' :: Name -> [(String, String)] -> Q [Dec]
mkAttrs' ty = fmap concat . mapM (uncurry (mkAttr ty))

mkAttr :: Name -> String -> String -> Q [Dec]
mkAttr ty name attr = do
  let n = mkName name
  x <- newName "x"
  b <- [| \y -> Attribute attr (pToJSRef y) |]
  return [ SigD n (AppT (AppT ArrowT (ConT ty)) (ConT ''Attribute))
         , FunD n [Clause [VarP x] (NormalB (AppE b (VarE x))) []]
         , PragmaD (InlineP n Inline FunLike AllPhases)
         ]

mkEventTypes :: Name -> [(String, [Name])] -> Q [Dec]
mkEventTypes base = fmap concat . mapM mk
  where
    mk (n, cls) = do
      let nn     = mkName n
          mkI cn = InstanceD [] (AppT (ConT cn) (ConT nn)) []
          insts  = map mkI (base : cls)
      jsr <- [t| JSRef |]
      return $ (NewtypeD [] nn [] (NormalC nn [(NotStrict, jsr)]) [''Typeable]) : insts

newtype CreatedEvents = CreatedEvents { unCreatedEvents :: [String] }
  deriving (Typeable)

addCreatedEvent :: String -> CreatedEvents -> CreatedEvents
addCreatedEvent ev (CreatedEvents es) = CreatedEvents (ev:es)

-- dcon must be a newtype constructor, not a data con
mkEvents :: Name -> [String] -> Q [Dec]
mkEvents dcon xs = fmap concat (mapM (\x -> mkEvent dcon x ("ev-"++x)) xs)

-- dcon must be a newtype constructor, not a data con
mkEvent :: Name -> String -> String -> Q [Dec]
mkEvent dcon name attr = do
  let n    = mkName name
      emsg = "GHCJS.VDOM.Internal.mkEvent: expected newtype constructor"
  i <- reify dcon
  dctyp <- case i of
    DataConI _ _ pn _ -> do
      pni <- reify pn
      case pni of
         TyConI (NewtypeD _ ctn _ _ _) -> return (ConT ctn)
         _                             -> error emsg
    _                 -> error emsg
  iou <- [t| IO () |]
  h <- newName "h"
  b <- [| mkEventAttr (fromString attr) |]
  let ht = AppT (AppT ArrowT dctyp) iou
  -- typ <- [t| (dctyp -> IO ()) -> Attribute |]
  qPutQ . maybe (CreatedEvents [name]) (addCreatedEvent name) =<< qGetQ
  return [ SigD n (AppT (AppT ArrowT ht) (ConT ''Attribute))
         , FunD n [Clause [VarP h] (NormalB (AppE (AppE b (ConE dcon)) (VarE h))) []]
         , PragmaD (InlineP n Inline FunLike AllPhases)
         ]

-- a must be a newtype of JSRef!
mkEventAttr :: JSString -> (JSRef -> a) -> (a -> IO ()) -> Attribute
mkEventAttr attr _wrap h =
  
  let e  = unsafeExportValue h
      h' = [js'| h$vdom.makeHandler(`e, false) |]
  in  h' `seq` Attribute attr h'
{-# INLINE mkEventAttr #-}

{-
eventLogger :: JSRef ()
eventLogger = [js'| function(ev) { console.log("event caught"); } |]
-}

-- generate a list of all events stored in the persistent TH state, created with mkEvent
mkDefaultEvents :: Q Exp
mkDefaultEvents = do
  evs <- maybe [] unCreatedEvents <$> qGetQ
  nil  <- [| [] |]
  cons <- [| (:) |]
  return $ foldl' (\xs e -> AppE (AppE cons (LitE . stringL $ e)) xs) nil evs
  
js_vnode :: JSString -> Attributes' -> Children' -> VNode
js_vnode tag (Attributes' props) (Children' children) =
  VNode [jsu'| h$vdom.v(`tag, `props, `children) |]
  --VNode [jsu'| new h$vdom.VNode(`tag, `props, `children) |]

getThunk :: J -> IO J
getThunk x = IO (js_getThunk x)

foreign import javascript unsafe "$1.hst"
  js_getThunk :: J -> State# RealWorld -> (# State# RealWorld, J #)

-- -----------------------------------------------------------------------------
{-|
   Export an arbitrary Haskell value to JS.

   be careful with these JSRef values, losing track of them will result in
   incorrect memory management. As long as we keep the values directly in
   a Property or VNode, the ghcjs-vdom extensible retention system will know
   where to find them.
 -}
unsafeExportValue :: a -> JSRef
unsafeExportValue x = js_export (unsafeCoerce x)
{-# INLINE unsafeExportValue #-}

{-|
   make a unique identifier that can be easily compared in JS
   if(objectIdent(o1) === objectIdent(o2) or both are NaN, then o1 and o2 are
   are the same Haskell value
 -}
objectIdent :: a -> JSIdent
objectIdent x = x `seq` js_makeObjectIdent (unsafeExportValue x)
{-
  unsafePerformIO . IO $ \s ->
  case makeStableName# x s of (# s', sn #) -> (# s', js_convertSn sn #)
-}
{-# INLINE objectIdent #-}
                             
foreign import javascript unsafe "$r = $1;" js_export    :: Any -> JSRef
foreign import javascript unsafe "$r = $1;" js_convertSn :: StableName# a -> JSIdent

foreign import javascript unsafe "h$makeStableName($1)" js_makeObjectIdent :: JSRef -> JSIdent
