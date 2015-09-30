{-# LANGUAGE QuasiQuotes, TemplateHaskell, FlexibleInstances #-}

module GHCJS.VDOM.Internal.Types where

import qualified Data.JSString as JSS
import           Data.String (IsString(..))

import           GHCJS.Foreign.QQ
import           GHCJS.Types
import qualified GHCJS.Prim.Internal.Build
import qualified GHCJS.Prim.Internal.Build as IB

import           GHCJS.VDOM.Internal.TH

import           Unsafe.Coerce

-- do not export the constructors for these, this ensures that the objects are opaque
-- and cannot be mutated
newtype VNode      = VNode      { unVNode      :: JSRef }
newtype VComp      = VComp      { unVComp      :: JSRef }
newtype DComp      = DComp      { unDComp      :: JSRef }
newtype Patch      = Patch      { unPatch      :: JSRef }
newtype VMount     = VMount     { unVMount     :: JSRef }

-- fixme: make newtype?
-- newtype JSIdent = JSIdent JSRef
-- newtype DOMNode = DOMNode JSRef
type JSIdent = JSRef
type DOMNode = JSRef


class Attributes a where
  mkAttributes :: a -> Attributes'
newtype Attributes' = Attributes' JSRef

data Attribute = Attribute JSString JSRef

class Children a where
  mkChildren :: a -> Children'
newtype Children' = Children' { unChildren :: JSRef }

instance Children Children' where
  mkChildren x = x
  {-# INLINE mkChildren #-}

instance Children () where
  mkChildren _ = Children' [jsu'| [] |]
  {-# INLINE mkChildren #-}

instance Children VNode where
  mkChildren (VNode v) =
    Children' [jsu'| [`v] |]
  {-# INLINE mkChildren #-}

mkTupleChildrenInstances ''Children 'mkChildren ''VNode 'VNode 'Children' [2..32]

instance Children [VNode] where
  mkChildren xs = Children' $ IB.buildArrayI (unsafeCoerce xs)
  {-# INLINE mkChildren #-}

instance Attributes Attributes' where
  mkAttributes x = x
  {-# INLINE mkAttributes #-}

instance Attributes () where
  mkAttributes _ = Attributes' [jsu'| {} |]
  {-# INLINE mkAttributes #-}

instance Attributes Attribute where
  mkAttributes (Attribute k v) =
    Attributes' (IB.buildObjectI1 (unsafeCoerce k) v)

instance Attributes [Attribute] where
  mkAttributes xs = Attributes' (IB.buildObjectI $
                                map (\(Attribute k v) -> (unsafeCoerce k,v)) xs)

mkTupleAttrInstances ''Attributes 'mkAttributes ''Attribute 'Attribute 'Attributes' [2..32]

instance IsString VNode where fromString xs = text (JSS.pack xs)

text :: JSString -> VNode
text xs = VNode [jsu'| h$vdom.t(`xs) |]
{-# INLINE text #-}
