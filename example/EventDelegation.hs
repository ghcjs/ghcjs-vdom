{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

{-
  virtual-dom binding demo, showing off event delegation
 -}

module Main where

import           Prelude hiding (div)

import           Control.Concurrent

import           Control.Monad ((<=<))

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
  cb <- syncCallback AlwaysRetain True (putStrLn "clicked me")
  cb2 <- syncCallback AlwaysRetain True (putStrLn "double clicked me")
  let newProps = click    cb 
               . dblclick cb2 
               $ props
  root <- createElement $ div newProps noChildren
  [js_| document.body.appendChild(`root); |]