{-# LANGUAGE TemplateHaskell, QuasiQuotes, LambdaCase, GHCForeignImportPrim #-}
{-|
   Code that deals with forcing thunks in virtual-dom trees. When
   computing a diff, the virtual-dom code returns a list of thunks
   found in the tree. The caller then forces the thunks and recurses
   into them to advance the diff computation, until all thunks have
   been evaluated.
 -}
module GHCJS.VDOM.Internal.Thunk where

import GHCJS.Foreign.QQ
import GHCJS.Prim

import Control.Exception
import Control.Monad

import GHC.Exts (Any)
import Unsafe.Coerce

import           GHCJS.VDOM.Internal       (j,J)
import qualified GHCJS.VDOM.Internal       as I
import           GHCJS.VDOM.Internal.Types

diff' :: J -> J -> IO J
diff' a b = do
  thunks <- [jsu| [] |]
  p <- [jsu| h$vdom.diff(`a, `b, `thunks) |]
  forceThunks thunks
  forcePatch p
  return p

forceThunks :: J -> IO ()
forceThunks thunks
  | [j| `thunks.length > 0 |] = fromJSArray thunks >>= mapM_ forceNode
  | otherwise                 = return ()
  where
    forceNode n = do
      forceThunkNode [j| `n.a |]
      forceThunkNode [j| `n.b |]
      patch <- diff' [j| `n.a.vnode |] [j| `n.b.vnode |]
      [jsu_| h$vdom.setThunkPatch(`n, `patch); |]

forceThunkNode :: J -> IO ()
forceThunkNode x =
  [jsu| `x && `x.hst && !`x.vnode |] >>= \case
    True -> do
      (VNode u) <- fmap unsafeCoerce . js_toHeapObject =<< I.getThunk x
      [jsu| `x.hst = null;
            `x.vnode = `u;
          |]
    _ -> return ()


forcePatch :: J -> IO ()
forcePatch p = do
  thunks <- [jsu| h$vdom.forcePatch(`p) |]
  forceTree [thunks]

forceTree :: [J] -> IO ()
forceTree [] = return ()
forceTree (x:xs) = do
  x' <- fromJSArray x
  ys <- forM x' $ \t -> do
    forceThunkNode t
    newThunks <- [jsu| [] |]
    [jsu_| h$vdom.forceTree(`t.vnode, `newThunks) |]
    return newThunks
  forceTree (filter (\a -> [jsu'| `a.length !== 0 |]) ys ++ xs)

foreign import javascript unsafe
  "$r = $1;" js_toHeapObject :: JSVal -> IO Any
