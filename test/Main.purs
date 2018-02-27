module Main where

import Control.Monad.Eff.Console
import Prelude

import Control.Alternative (empty)
import Control.Plus (empty)
import DOM.HTML.Event.EventTypes (show)

import Data.List (List(..), filter, head)
import Data.Maybe (Maybe)
import Data.List.Lazy.Types(cons)
import Math (sqrt, pi)

diagonal w h = sqrt ( w*w + h*h )

area r = pi*r*r

main = logShow (area 3.0)


type Entry =
  { firstName :: String
  , lastName  :: String
  , address   :: Address
  }

type Address =
  { street :: String
  , city   :: String
  , state  :: String
  }


type AddressBook = List Entry

showAddress :: Address -> String
showAddress address = address.street <> "," <> address.city <> "," <> address.state

showEntry :: Entry -> String
showEntry entry = entry.firstName <> ","  <> entry.lastName <> "," <> showAddress entry.address


emptyBook :: AddressBook
emptyBook = empty


insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = cons