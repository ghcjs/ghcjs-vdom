{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

{-
  virtual-dom binding demo, showing off event delegation
 -}

module Main where

import           Prelude hiding (div)

import           Control.Concurrent

import           System.IO

import           GHCJS.VDOM
import           GHCJS.VDOM.QQ
import           GHCJS.Foreign
import           GHCJS.Foreign.QQ
import           GHCJS.Types

import           System.IO.Unsafe


main :: IO ()
main = do

  -- a quick example that demonstrates event delegation.
  -- we will probably want a cleaner API For this. but for now
  -- it suffices to demonstrate the way it works.
  let props = [pr|style:{backgroundColor:'red', width: '20px', height: '20px'}|]
  cb <- syncCallback AlwaysRetain True (putStrLn "hello, world")
  newProps <- setEventHandler props "click" cb
  root <- createElement $ div newProps noChildren
  [js_| document.body.appendChild(`root); |]


