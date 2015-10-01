{-|
   Unsafe API for constructing Children' and Attributes', useful for
   writing new Children and Attributes instances
 -}
module GHCJS.VDOM.Unsafe ( Attributes(..),       Children(..)
                         , Attributes',          Children'
                         , unsafeToAttributes,   unsafeToChildren
                         , Attribute(..) ) where

import GHCJS.Types

import GHCJS.VDOM.Internal.Types

{-|
   Convert a JSRef, which must be a JS object, to Attributes'. The object
   may not be mutated.

   FIXME what do we need to always be able to find Haskell callbacks?
 -}
unsafeToAttributes :: JSRef -> Attributes'
unsafeToAttributes = Attributes'

{-|
   Convert a JSRef, which must be an array of virtual-dom nodes
   to Children'. The array and the child nodes may not be mutated.

   note: All nodes must either be a virtual node or an instance of HSThunk,
   if you use other node types, the extensible retention (see scanTree in
   virtual-dom/lib.require.js) must be extended.
 -}
unsafeToChildren :: JSRef -> Children'
unsafeToChildren = Children'
