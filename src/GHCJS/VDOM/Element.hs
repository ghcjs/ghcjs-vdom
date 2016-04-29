{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module GHCJS.VDOM.Element ( custom
                          , customNS
                          , customSVG
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


customNS :: (Attributes a, Children c) => JSString -> JSString -> a -> c -> VNode
customNS ns tag a c = I.mkVNodeNS ns tag a c
{-# INLINE customNS #-}



customSVG :: (Attributes a, Children c) => JSString -> a -> c -> VNode
customSVG tag a c = I.mkVNodeSVG tag a c
{-# INLINE customSVG #-}







