{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-
  ghcjs-vdom example, demonstrating components
 -}

module Main where

import           GHCJS.Foreign.QQ
import           GHCJS.Types

import qualified GHCJS.VDOM.Component    as C
import qualified GHCJS.VDOM.DOMComponent as D
import qualified GHCJS.VDOM.Attribute    as A
import qualified GHCJS.VDOM.Element      as E
import qualified GHCJS.VDOM.Event        as Ev
import           GHCJS.VDOM

import qualified Data.JSString.Int       as JSS

import           Data.IORef
import           Data.Monoid
import qualified Data.Map as M

import           Control.Concurrent
import           Control.Monad

import           System.IO

{-
  example virtual-dom component:
  a simple counter component that increments on click
 -}
data Counter = Counter { counterComp :: VComp
                       , counterKey  :: Int
                       , getCount    :: IO Int
                       }

mkCounter :: JSString -> Int -> IO Counter
mkCounter description startValue = do
  val <- newIORef startValue
  let description' = E.text (description <> ": ")
  c <- fixIO $ \c ->
      -- example only: diff and patch should really done through a Renderer
      let repaint   = C.render c >>= C.diff c >>= C.patch c
          increment = modifyIORef' val (+1) >> repaint >> return ()
      in  C.mkComponent $ do
            v <- readIORef val
            return $ E.div (A.class_ "counter", Ev.click (const increment))
                           [description', E.text (JSS.decimal v)]
  return $ Counter c startValue (readIORef val)

-- example DOM component
data Scroller = Scroller { scrollerComp :: DComp
                         , scrollerKey  :: Int
                         }

mkScroller :: JSString -> Int -> IO Scroller
mkScroller txt k = do
  mounts <- newIORef M.empty
  let mountScroller m = do
        (n::JSRef) <- [js| document.createElement('div') |]
        (t::JSRef) <- [js| document.createTextNode(`txt) |]
        [jsu_| `n.appendChild(`t); |]
        thr <- forkIO . forever $ do
           threadDelay 200000
           [jsu_| `t.data = `t.data.substr(1) + `t.data.substr(0,1); |]
        atomicModifyIORef mounts ((,()) . M.insert m thr)
        return n
      unmountScroller m _ = do
        Just thr <- M.lookup m <$> readIORef mounts
        killThread thr
        atomicModifyIORef mounts ((,()) . M.delete m)
        return ()
  c <- D.mkComponent mountScroller unmountScroller
  return (Scroller c k)

renderCounterList :: [Counter] -> VNode
renderCounterList counters =
  E.ul (A.class_ "counterList")
       (map (\c -> E.li (A.key (counterKey c)) (C.toNode (counterComp c)))
            counters)

renderScrollerList :: [Scroller] -> VNode
renderScrollerList scrollers =
  E.ul (A.class_ "scrollerList")
       (map (\s -> E.li (A.key (scrollerKey s)) (D.toNode (scrollerComp s)))
            scrollers)

render :: [Counter] -> [Scroller] -> VNode
render counters scrollers =
  E.div () [renderCounterList counters, renderScrollerList scrollers]

main :: IO ()
main = do
  Ev.initEventDelegation Ev.defaultEvents
  root <- [js| document.createElement('div') |]
  [js_| document.body.appendChild(`root); |]
  counters <- mapM (\i -> mkCounter ("counter " <> JSS.decimal i) i) [1..10]
  scrollers <- mapM (\i -> mkScroller ("scroller " <> JSS.decimal i) i) [1..10]
  m <- mount root (render counters scrollers)
  rotateComponents m 11 counters scrollers

rotateComponents :: VMount -> Int -> [Counter] -> [Scroller] -> IO ()
rotateComponents m n counters scrollers = do
  threadDelay 1000000
  newCounter <- mkCounter ("counter " <> JSS.decimal n) n
  newScroller <- mkScroller ("scroller " <> JSS.decimal n) n
  let scrollers' = tail scrollers ++ [newScroller]
      counters'  = tail counters ++ [newCounter]
  void $ diff m (render counters' scrollers') >>= patch m
  rotateComponents m (n+1) counters' scrollers'
