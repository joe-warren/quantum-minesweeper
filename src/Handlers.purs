module Handlers 
( onContextMenu
) where

import Prelude

import Data.Maybe
import Halogen.HTML.Properties as HP
import Web.Event.Event (Event, EventType(..), preventDefault, stopPropagation)
import Halogen.HTML.Events as HE

onContextMenu :: forall r i. (Event -> i) -> HP.IProp (onContextMenu :: Event | r) i
onContextMenu = HE.handler (EventType "contextmenu")