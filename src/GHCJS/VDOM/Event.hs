{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module GHCJS.VDOM.Event ( initEventDelegation
                        , defaultEvents
--                        , target
                        , stopPropagation
                        , stopImmediatePropagation
                        , preventDefault
                          
                          -- * mouse
                        , MouseEvent
                        , click
                        , dblclick
                        , mousedown
                        , mouseenter
                        , mouseleave
                        , mousemove
                        , mouseout
                        , mouseover
                        , mouseup
                          --
                        , button
                        , buttons
                        , clientX
                        , clientY
                           
                          -- * keyboard
                        , KeyboardEvent
                        , keydown
                        , keypress
                        , keyup
                          --
                        , key
                        , ctrlKey
                        , metaKey
                        , shiftKey
                          
                          -- * drag
                        , DragEvent
                        , drag
                        , dragend
                        , dragenter
                        , dragleave
                        , dragover
                        , dragstart
                          
                          -- * focus
                        , FocusEvent
                        , focus
                        , blur

                          -- * ui
                        , UIEvent
                        , resize
                        , scroll
                        , select
                        , unload
                          
                          -- * wheel
                        , WheelEvent
                        , wheel
                          --
                        , deltaX
                        , deltaY
                        , deltaZ
                        , deltaMode
                          
                          -- * generic
                        , Event
                        , submit
                        , change
                        ) where

import Data.Coerce

import Unsafe.Coerce

import GHCJS.Prim
import GHCJS.Types
import GHCJS.Foreign.QQ

import GHCJS.VDOM.Internal

-- | call this to initialize the virtual-dom event handling system
initEventDelegation :: [JSString] -> IO ()
initEventDelegation eventTypes = do
  a <- toJSArray (unsafeCoerce eventTypes)
  [jsu_| h$vdom.initDelegator(`a); |]

class Coercible a JSRef => Event_ a
class Event_ a          => KeyModEvent_ a
class Event_ a          => MouseEvent_ a
class Event_ a          => FocusEvent_ a

mkEventTypes ''Event_ [ ("MouseEvent",    [''MouseEvent_])
                      , ("KeyboardEvent", [''KeyModEvent_])
                      , ("FocusEvent",    [''FocusEvent_])
                      , ("DragEvent",     [])
                      , ("WheelEvent",    [])
                      , ("UIEvent",       [])
                      , ("Event",         [])
                      ]

mkEvents 'MouseEvent [ "click", "dblclick", "mousedown", "mouseenter"
                     , "mouseleave", "mousemove", "mouseout"
                     , "mouseover", "mouseup"
                     ]

mkEvents 'KeyboardEvent [ "keydown", "keypress", "keyup" ]

mkEvents 'DragEvent [ "drag", "dragend", "dragenter", "dragleave"
                    , "dragover", "dragstart" ]

mkEvents 'FocusEvent [ "focus", "blur" ]

mkEvents 'UIEvent [ "resize", "scroll", "select", "unload" ]

mkEvents 'WheelEvent [ "wheel" ]

mkEvents 'Event [ "submit", "change" ]

er :: Event_ a => (JSRef -> b) -> a -> b
er f x = f (coerce x)

-- -----------------------------------------------------------------------------

-- this contains all event types added with mkEvents
defaultEvents :: [JSString]
defaultEvents = $(mkDefaultEvents)


-- target :: Event_ a => a -> VNode
-- target e = undefined

stopPropagation :: Event_ a => a -> IO ()
stopPropagation = er $ \e -> [jsu_| `e.stopPropagation(); |]
{-# INLINE stopPropagation #-}

stopImmediatePropagation :: Event_ a => a -> IO ()
stopImmediatePropagation = er $ \e -> [jsu_| `e.stopImmediatePropagation(); |]
{-# INLINE stopImmediatePropagation #-}

preventDefault :: Event_ a => a -> IO ()
preventDefault = er $ \e -> [jsu_| `e.preventDefault(); |]
{-# INLINE preventDefault #-}

ctrlKey :: KeyModEvent_ a => a -> Bool
ctrlKey = er $ \e -> [jsu'| `e.ctrlKey |]
{-# INLINE ctrlKey #-}

metaKey :: KeyModEvent_ a => a -> Bool
metaKey = er $ \e -> [jsu'| `e.ctrlKey |]
{-# INLINE metaKey #-}

shiftKey :: KeyModEvent_ a => a -> Bool
shiftKey = er $ \e -> [jsu'| `e.ctrlKey |]
{-# INLINE shiftKey #-}

key :: KeyboardEvent -> JSString
key = er $ \e -> [jsu'| `e.key |]
{-# INLINE key #-}

button :: MouseEvent_ a => a -> Int
button = er $ \e -> [jsu'| `e.button |]
{-# INLINE button #-}

buttons :: MouseEvent_ a => a -> Int
buttons = er $ \e -> [jsu'| `e.buttons |]
{-# INLINE buttons #-}

deltaX :: WheelEvent -> Double
deltaX = er $ \e -> [jsu'| `e.deltaX |]
{-# INLINE deltaX #-}

deltaY :: WheelEvent -> Double
deltaY = er $ \e -> [jsu'| `e.deltaY |]
{-# INLINE deltaY #-}

deltaZ :: WheelEvent -> Double
deltaZ = er $ \e -> [jsu'| `e.deltaZ |]
{-# INLINE deltaZ #-}

deltaMode :: WheelEvent -> Double
deltaMode = er $ \e -> [jsu'| `e.deltaMode |]
{-# INLINE deltaMode #-}

clientX :: MouseEvent -> Int
clientX = er $ \e -> [jsu'| `e.clientX|0 |]
{-# INLINE clientX #-}

clientY :: MouseEvent -> Int
clientY = er $ \e -> [jsu'| `e.clientY|0 |]
{-# INLINE clientY #-}
