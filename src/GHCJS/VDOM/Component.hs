{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE UnliftedFFITypes #-}

module GHCJS.VDOM.Component ( VComp
                            , toNode
                            , mkComponent
                            , render
                            , diff
                            , patch
                            ) where

import           Control.Monad

import           GHCJS.Foreign.QQ

import           GHCJS.VDOM.Internal.Types

import qualified GHCJS.VDOM.Internal       as I
import           GHCJS.VDOM.Internal       (j)
import qualified GHCJS.VDOM.Internal.Thunk as I


toNode :: VComp -> VNode
toNode (VComp v) = VNode v
{-# INLINE toNode #-}

mkComponent :: IO VNode -> IO VComp
mkComponent r = do
  let renderE = I.unsafeExportValue r
  c <- VComp <$> [jsu| h$vdom.c(`renderE, null, null, null) |]
  void $ patch c =<< diff c =<< render c
  return c

foreign import javascript unsafe "$1.hsRender"
  js_hsRender :: VComp -> (# IO VNode #)

render :: VComp -> IO VNode
render c = case js_hsRender c of (# x #) -> x
{-# INLINE render #-}

diff :: VComp -> VNode -> IO Patch
diff (VComp c) (VNode v) = do
  thunks <- [jsu| [] |]
  patch  <- [jsu| `c.diff(`v, `thunks) |]
  I.forceThunks thunks
  I.forcePatch [j| `patch.patch |]
  return (Patch patch)
{-# INLINE diff #-}

patch :: VComp -> Patch -> IO Bool
patch (VComp c) (Patch p) = [jsu| `c.patch(`p) |]
{-# INLINE patch #-}

