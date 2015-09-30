{-# LANGUAGE QuasiQuotes #-}

{-|
   Bindings for the virtual-dom library.

   The virtual-dom diff function has been changed slightly to allow it to work
   with full functionality in asynchronous threads.

   It's possible to implement the bindings without the modifications at the
   cost of tail-call optimization and preemptive threading in the diff, by
   recursively forcing the thunks in synchronous threads.
 -}

module GHCJS.VDOM ( Attributes, Children
                  , Attributes', Children'
                  , VMount, VNode, VComp, DComp, Patch, DOMNode
                  , mount, unmount
                  , diff, patch
                  , memo, memoKey
                  ) where

import GHCJS.Types
import GHCJS.Foreign.QQ
import GHCJS.Prim
import GHCJS.Marshal.Pure

import Control.Monad
import Data.Coerce

import System.IO.Unsafe

import           GHCJS.VDOM.Internal.Types
import           GHCJS.VDOM.Internal.Thunk
import           GHCJS.VDOM.Internal       (j,J)
import qualified GHCJS.VDOM.Internal       as I

class MemoNode a where memoNode :: (J, [JSIdent], a) -> a

instance MemoNode VNode
  where
    memoNode (_,[],a) = a
    memoNode (k,xs,v) =
      let vd     = I.unsafeExportValue v
          xs1    = unsafePerformIO (toJSArray $ coerce xs)
      in VNode [j| h$vdom.th(`vd, `xs1, `k, true) |]
    {-# INLINE memoNode #-}

instance MemoNode b => MemoNode (a -> b)
  where
    memoNode (k,xs,f) = \a -> memoNode (k, I.objectIdent a:xs, f a)
    {-# INLINE memoNode #-}

memoKey :: MemoNode a => JSString -> a -> a
memoKey k = memo' (pToJSRef k)
{-# NOINLINE memoKey #-}

memo :: MemoNode a => a -> a
memo = memo' [j| $r = null; |]
{-# NOINLINE memo #-}

memo' :: MemoNode a => J -> a -> a
memo' k f = memoNode (k,[I.objectIdent f],f)
{-# INLINE memo' #-}

{-|
   Mount a virtual-dom tree in the real DOM. The mount point can be updated
   with patch.
-}
mount :: DOMNode -> VNode -> IO VMount
mount n v = do
  m <- VMount <$> [js| h$vdom.mount(`n) |]
  void $ patch m =<< diff m v
  return m
{-# INLINE mount #-}

{-|
   Remove a virtual-dom tree from the document. It's important to use
   unmount rather than removing the mount point any other way since this
   releases all associated Haskell data structures.
 -}
unmount :: VMount -> IO ()
unmount (VMount m) = [jsu_| h$vdom.unmount(`m); |]
{-# INLINE unmount #-}

{-|
   Compute a patch to update the mounted tree to match the virtual-dom tree
 -}
diff :: VMount -> VNode -> IO Patch
diff (VMount m) (VNode v) = do
  thunks <- [jsu| [] |]
  patch  <- [jsu| `m.diff(`v, `thunks) |]
  forceThunks thunks
  forcePatch [j| `patch.patch |]
  return (Patch patch)
{-# INLINE diff #-}

{-|
   Apply a patch to a mounted virtual-dom tree. Fails if the tree has already
   been patched after the diff was computed.
 -}
patch :: VMount -> Patch -> IO Bool
patch (VMount m) (Patch p) = [jsu| `m.patch(`p); |]
{-# INLINE patch #-}



