{-# LANGUAGE TypeFamilies, QuasiQuotes, ScopedTypeVariables #-}
{-|
   Some utilities to manage a render queue for ghcjs-vdom components.

   Diffing is performed in an asynchronous background thread. When a
   patch is ready, an animationframe is requested to apply the update.

   Using the render queue is optional, directly calling diff and patch
   for the components is a lower level way to update the document.
 -}

module GHCJS.VDOM.Render ( Renderer
                         , render
                         , mkRenderer
                         ) where

import           GHCJS.Foreign.Callback
import           GHCJS.Foreign.QQ
import           GHCJS.Types

import           GHCJS.VDOM.Internal.Types
import qualified GHCJS.VDOM.Component as C
import qualified GHCJS.VDOM as V

import           JavaScript.Web.AnimationFrame

import           Control.Concurrent
import qualified Control.Exception as E
import           Control.Monad

import           Data.IORef
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Typeable

class Renderable a where
  type Render a
  doRender :: Renderer -> a -> Render a

instance Renderable VComp where
  type Render VComp = IO ()
  doRender r c@(VComp vcj) =
    enqueueRender r vcj (C.render c >>= C.diff c >>= addPatch vcj) (flushPatch vcj)

instance Renderable VMount where
  type Render VMount = (VNode -> IO ())
  doRender r vm@(VMount vmj) vn =
    enqueueRender r vmj (V.diff vm vn >>= addPatch vmj) (flushPatch vmj)

render :: Renderable a => Renderer -> a -> Render a
render r x = doRender r x

mkRenderer :: IO Renderer
mkRenderer = do
  r <- Renderer <$> newIORef M.empty
                <*> newIORef M.empty
                <*> newChan
  void (forkIO (renderThread r))
  return r

data Renderer = Renderer
  { renderPending      :: IORef (Map Double (IO (), IO ()))
  , renderFlushPending :: IORef (Map Double (IO ()))
  , renderQueue        :: Chan Double
  } deriving (Typeable)

renderThread :: Renderer -> IO ()
renderThread r = forever $ do
  k <- readChan (renderQueue r)
  Just (compute, flush) <-
    atomicModifyIORef (renderPending r) (\m -> (M.delete k m, M.lookup k m))
  let actions = do
        compute
        m <- atomicModifyIORef (renderFlushPending r)
                               (\m -> (M.insert k flush m, m))
        when (M.null m) (void $ inAnimationFrame ThrowWouldBlock
                                                 (flushPatches r))
  actions`E.catch` \(_::E.SomeException) -> return ()

flushPatches :: Renderer -> IO ()
flushPatches r =
  mapM_ (\m -> m `E.catch` \(_::E.SomeException) -> return ()) =<<
    atomicModifyIORef (renderFlushPending r)
                      (\m -> (M.empty, M.elems m))

enqueueRender :: Renderer
              -> JSRef
              -> IO ()
              -> IO ()
              -> IO ()
enqueueRender r renderable compute flush = do
  k <- renderableKey renderable
  m <- atomicModifyIORef (renderPending r)
                         (\m -> (M.insert k (compute, flush) m, m))
  when (M.notMember k m) (writeChan (renderQueue r) k)

renderableKey :: JSRef -> IO Double
renderableKey r = [jsu| `r._key |]

addPatch :: JSRef -> Patch -> IO ()
addPatch r (Patch p) = [jsu_| `r.addPatch(`p); |]

flushPatch :: JSRef -> IO ()
flushPatch r = [jsu_| `r.patch(null); |]
