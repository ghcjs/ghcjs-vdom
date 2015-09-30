{-# LANGUAGE QuasiQuotes, OverloadedStrings, BangPatterns #-}

{-
  virtual-dom bindings demo, rendering a large pixel grid with a bouncing red
  square. the step and patch are calculated asynchronously, the update is
  batched in an animation frame
 -}

module Main where

import           Control.Monad

import           Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import qualified Data.JSString as JSS

import           GHCJS.VDOM
import           GHCJS.VDOM.QQ
import qualified GHCJS.VDOM.Element as E
import qualified GHCJS.VDOM.Attribute as A

import           GHCJS.Foreign.Callback
import           GHCJS.Foreign.QQ
import           GHCJS.Types

import           JavaScript.Web.AnimationFrame (inAnimationFrame)

red :: JSString
red = "pixel-red"

white :: JSString
white = "pixel-white"

type Pixels = IntMap (IntMap JSString)

setPixel :: Int -> Int -> JSString -> Pixels -> Pixels
setPixel x y c p =
  let r  = p IM.! y
      r' = IM.insert x c r
  in  r' `seq` IM.insert y r' p

data State = State { x  :: !Int, y  :: !Int
                   , dx :: !Int, dy :: !Int
                   , w  :: !Int, h  :: !Int
                   , pixels :: !Pixels
                   }

mkState :: Int -> Int -> Int -> Int -> State
mkState w h x y = State x y 1 1 w h pix
  where
    pix     = IM.fromList $ map row [0..h-1]
    row n   = (n, IM.fromList (map (col n) [0..w-1]))
    col n m = (m, if (m,n)==(x,y) then red else white)

step :: State -> State
step (State x y dx dy w h p) =
  let dx' = if x==0 then 1 else if x==(w-1) then -1 else dx
      dy' = if y==0 then 1 else if y==(h-1) then -1 else dy
      x'  = x+dx'
      y'  = y+dy'
      p'  = setPixel x' y' red (setPixel x y white p)
   in State x' y' dx' dy' w h p'

cls :: JSString -> Attributes'
cls name = [att| className: name |]

render :: State -> VNode
render s = E.div (cls "state") [ch|pixelDiv,numDiv|]
    where
      xd       = textDiv (y s)
      yd       = textDiv (x s)
      numDiv   = E.div (cls "numeric") [ch|xd,yd|]
      pixelDiv = E.div (cls "pixels")
          (map (renderRowM (w s) . (pixels s IM.!)) [0..h s-1])

textDiv :: Show a => a -> VNode
textDiv x = E.div () [ch|c|]
  where
    c = E.text . JSS.pack . show $ x

renderRowM !w !r = memo renderRow w r

renderRow :: Int -> IntMap JSString -> VNode
renderRow w r =
  E.div (A.class_ "row", A.lang "EN") (map (renderPixelM r) [0..w-1])

renderPixelM !r !c = memo renderPixel r c

renderPixel :: IntMap JSString -> Int -> VNode
renderPixel r c = E.div (cls (r IM.! c)) ()

animate :: VMount -> State -> IO ()
animate m s =
  let s' = step s
      r' = render s'
  in do p <- diff m r'
        void $ inAnimationFrame ContinueAsync (\_ -> patch m p >> animate m s')

main :: IO ()
main = do
  root <- [js| document.createElement('div') |]
  [js_| document.body.appendChild(`root); |]
  let s = mkState 167 101 10 20
  m <- mount root (E.div () ())
  animate m s


