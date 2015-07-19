{-# LANGUAGE QuasiQuotes, OverloadedStrings, BangPatterns #-}

{-
  ghcjs-vdom example, demonstrating the render queue
 -}

module Main where

import           Control.Concurrent
import           Control.Monad

import           Data.IORef
import           Data.Monoid

import           GHCJS.Foreign.QQ
import           GHCJS.Types

import qualified GHCJS.VDOM.Component    as C
import qualified GHCJS.VDOM.Attribute    as A
import qualified GHCJS.VDOM.Element      as E
import qualified GHCJS.VDOM.Render       as R
import           GHCJS.VDOM

import qualified Data.JSString.Int       as JSS


-- a component with a slowed down rendering function
mkSlow :: JSString -> Int -> IO VComp
mkSlow descr delay = do
  count <- newIORef (0::Int)
  let descr' = E.text (descr <> " " <> JSS.decimal delay <> ": ")
  C.mkComponent $ do
    threadDelay delay
    c <- atomicModifyIORef count (\x -> let x' = x+1 in (x',x'))
    return $ E.div (A.class_ "slow") [descr', E.text (JSS.decimal c)]

main :: IO ()
main = do
  root <- [js| document.createElement('div') |]
  [js_| document.body.appendChild(`root); |]
  slows1 <- mapM (mkSlow "slow1") [10000, 50000, 100000, 300000]
  slows2 <- mapM (mkSlow "slow2") [10000, 50000, 100000]
  void $ mount root (E.div () (map C.toNode (slows1++slows2)))
  r1 <- R.mkRenderer
  r2 <- R.mkRenderer
  forever $ do
    mapM_ (R.render r1) slows1
    mapM_ (R.render r2) slows2
    threadDelay 10000



