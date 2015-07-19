{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module GHCJS.VDOM.Element ( custom
                          , text
                          , module GHCJS.VDOM.Element.Builtin
                          ) where

import           Data.JSString (JSString)

import qualified GHCJS.VDOM.Internal as I
import           GHCJS.VDOM.Internal.Types

import           GHCJS.VDOM.Element.Builtin

custom :: (Attributes a, Children c) => JSString -> a -> c -> VNode
custom tag a c = I.mkVNode tag a c
{-# INLINE custom #-}
