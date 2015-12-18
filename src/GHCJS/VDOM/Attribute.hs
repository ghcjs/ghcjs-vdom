{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module GHCJS.VDOM.Attribute ( Attribute
                            , Attributes
                            , custom
                              -- * some predefined attributes
                            , class_
                            , style
                            , id
                            , href
                            , alt
                            , src
                            , name
                            , target
                            , value
                            , width
                            , height
                            , title
                            , lang
                            , type_
                            , key -- virtual-dom identifiers
                            ) where

import Prelude hiding (id)

import GHCJS.Types

import GHCJS.VDOM.Internal.Types
import GHCJS.VDOM.Internal

custom :: JSString -> JSVal -> Attribute
custom = Attribute

mkAttrs ''JSString [ "id", "href", "src", "alt", "title", "style"
                   , "lang", "name", "target", "value"
                   ]

mkAttrs' ''JSString [ ("class_", "className")
                    , ("type_", "type")
                    ]

mkAttrs ''Int [ "key", "width", "height" ]
