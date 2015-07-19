{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module GHCJS.VDOM.Attribute ( Attribute
                            , Attributes
                              -- * some predefined attributes
                            , class_
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

mkAttrs ''JSString [ "id", "href", "src", "alt", "title"
                   , "lang", "name", "target", "value"
                   ]

mkAttrs' ''JSString [ ("class_", "className")
                    , ("type_", "type")
                    ]

mkAttrs ''Int [ "key", "width", "height" ]
