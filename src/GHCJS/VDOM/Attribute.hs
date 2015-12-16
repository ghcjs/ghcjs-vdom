{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module GHCJS.VDOM.Attribute ( Attribute
                            , Attributes
                            , mkAttributeFromList
                            , mkAttribute
                              -- * some predefined attributes
                            , class_
                            , id
                            , href
                            , alt
                            , src
                            , name
                            , target
                            , value
                            , style
                            , width
                            , height
                            , title
                            , lang
                            , type_
                            , key -- virtual-dom identifiers
                            ) where

import Prelude hiding (id)

import GHCJS.Types
import qualified GHCJS.Prim.Internal.Build as IB
import GHCJS.VDOM.Internal.Types
import GHCJS.VDOM.Internal
import Unsafe.Coerce
mkAttrs ''JSString [ "id", "href", "src", "alt", "title"
                   , "lang", "name", "target", "value", "style"
                   ]

mkAttrs' ''JSString [ ("class_", "className")
                    , ("type_", "type")
                    ]

mkAttrs ''Int [ "key", "width", "height" ]

mkAttribute :: JSString -> JSVal -> Attribute
mkAttribute = Attribute

-- | For Proper Attributes in VDOM they must turn into an object
mkAttributeFromList :: JSString -> [Attribute] -> Attribute
mkAttributeFromList attrObjName attrList= mkAttribute attrObjName . IB.buildObjectI
                           . fmap (\(Attribute k v) -> (unsafeCoerce k,v)) $ attrList
