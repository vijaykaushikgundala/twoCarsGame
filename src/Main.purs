module Main where

import Control.Monad.Eff.Random
import Data.String
import Data.String
import Prelude
import Types
import UI.Elements
import UI.Events
import UI.Properties

import Control.Comonad.Store (store)
import Control.Monad.Cont (lift)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Exception (stack)
import Control.Monad.List.Trans (scanl)
import Control.Monad.RWS (get, state)
import Control.Plus ((<|>))
import DOM.HTML.Event.ErrorEvent (lineNo)
import DOM.HTML.HTMLInputElement (step)
import DOM.HTML.History (back)
import Data.Array (group, concat, (!!))
import Data.Enum (downFrom)
import Data.Int (ceil, round, toNumber)
import Data.Number.Format (toString)
import Data.Ord (signum)
import FRP as F
import FRP.Behavior (sample)
import FRP.Behavior.Time (millisSinceEpoch)
import FRP.Event as E
import FRP.Event.Keyboard (down)
import FRP.Event.Mouse (move)
import FRP.Event.Time (animationFrame)
import Halogen.VDom.Types (graft)
import UI.Core (MEvent, AttrValue(..), Attr(..), Prop)
import UI.Util (done)
import UI.Util as U
import Data.Maybe

foreign import click :: MEvent
foreign import change :: MEvent
foreign import logAny :: forall a e. a -> Eff e Unit

widget state = relativeLayout
              [ id_ "10"
              , height "match_parent"
              , width "match_parent"
              , gravity "center"
              , orientation "vertical"
              ]
              [
                  linearLayout
                  [id_ "9"
                  , height "match_parent"
                  , width "600"
                  , background "#FFB6C1"
                  , orientation "horizontal"
                  ]
                  [
                    linearLayout
                    [id_ "1"
                    , height "match_parent"
                    , width "150"
                    , background "#929402"
                    , orientation "horizontal"
                    , gravity "center_horizontal"
                    ][]
                    ,
                    linearLayout
                    [id_ "2"
                    , height "match_parent"
                    , width "150"
                    , background "#008201"
                    , orientation "horizontal"
                    ][]
                    ,
                    linearLayout
                    [id_ "3"
                    , height "match_parent"
                    , width "150"
                    , background "#000030"
                    , orientation "horizontal"
                    ][]
                    ,
                    linearLayout
                    [id_ "4"
                    , height "match_parent"
                    , width "150"
                    , orientation "horizontal"
                    ][]
                  ],
                  imageView
                  [id_ "99"
                  , height "100"
                  , width "100"
                  , imageUrl "redCar"
                  , margin (state.marginRed)
                  ],
                  imageView
                  [id_ "999"
                  , height "100"
                  , width "100"
                  , imageUrl "blueCar"
                  , margin (state.marginBlue)
                  ],
                  imageView
                  [id_ "77"
                  , height "50"
                  , width "50"
                  , imageUrl "redSquare"
                  , margin (state.marginRedSquare)
                  ],
                  imageView
                  [id_ "777"
                  , height "50"
                  , width "50"
                  , imageUrl "blueSquare"
                  , margin (state.marginBlueSquare)
                  ],
                  textView
                  [id_ "text"
                  , height "100"
                  , width "100"
                  , text (state.text)
                  , color "#FF0000"
                  , fontStyle "Source Sans Pro-Regular"
                  , gravity "center"
                  , margin "300,300,20,20"
                  ],
                  textView
                  [id_ "11111"
                  , height "200"
                  , width "200"
                  , text "Your Score is:"
                  , color "#00FF00"
                  , fontStyle "Source Sans Pro-Regular"
                  , textSize "30"
                  , gravity "center"
                  , margin "600,200,20,20"
                  ],
                  textView
                  [id_ "score"
                  , height "200"
                  , width "200"
                  , text (state.score)
                  , color "#00FF00"
                  , textSize "30"
                  , fontStyle "Source Sans Pro-Regular"
                  , gravity "center"
                  , margin "600,300,20,20"
                  ]
                ]

getMargin :: Number -> Number -> Number -> Number -> String
getMargin a b c d = toString a <> "," <> toString b <> "," <> toString c <> "," <> toString d


getSpeed  = do
  a1 <- randomRange 4.0 7.0
  let b1 = round a1
  pure b1

getSquareWidth color = do
  a <- randomRange 0.0 2.0
  let b = round a
  if color == "red"
    then do
      if b == 0
        then do
          pure 40.0
          else
            pure 200.0
    else
      if b == 0
        then do
          pure 350.0
          else 
            pure 510.0


squareInitize color = do
  state <- U.getState
  if color == "red"
    then do
      rS <- getSpeed
      state <- U.updateState "speedR" (toNumber rS)
      rW <- getSquareWidth "red"
      state <- U.updateState "redSquareY" 40.0
      state <- U.updateState "redSquareX" rW
      let marginRedSquare =  getMargin state.redSquareX state.redSquareY 40.0 40.0
      state <- U.updateState "marginRedSquare" marginRedSquare
      pure state
      else do
        bS <- getSpeed
        bW <- getSquareWidth "blue"
        state <- U.updateState "speedB" (toNumber bS)
        state <- U.updateState "blueSquareX" bW
        state <- U.updateState "blueSquareY" 40.0
        let marginBlueSquare =  getMargin state.blueSquareX state.blueSquareY 40.0 40.0
        state <- U.updateState "marginBlueSquare" marginBlueSquare
        pure state

  
main = do
  --- Init State {} empty record--
  U.initializeState

  --- Update State ----
  state <- squareInitize "red" 
  state <- squareInitize "blue" 
  state <- U.updateState "score" 0.0
  state <- U.updateState "text" ""
  state <- U.updateState "marginRedX" 20.0
  state <- U.updateState "marginBlueX" 320.0
  let marginRedW = getMargin state.marginRedX 600.0 20.0 20.0
  let marginBlueW = getMargin state.marginBlueX 600.0 20.0 20.0
  state <- U.updateState "marginBlue" marginBlueW
  state <- U.updateState "marginRed" marginRedW

  ---- Render Widget ---
  U.render (widget state) listen

  pure unit

eval dummy = do
  state <- U.getState
  let redSquareY = toNumber state.redSquareY + toNumber state.speedR
  let blueSquareY = toNumber state.blueSquareY + toNumber state.speedB
  let score = state.score
  state <- U.updateState "redSquareY" redSquareY
  state <- U.updateState "blueSquareY" blueSquareY
  let marginRedSquare =  getMargin state.redSquareX state.redSquareY 40.0 40.0
  let marginBlueSquare =  getMargin state.blueSquareX state.blueSquareY 40.0 40.0
  state <- U.updateState "marginRedSquare" marginRedSquare
  state <- U.updateState "marginBlueSquare" marginBlueSquare
  if (state.redSquareY > 610.0 && state.redSquareX == state.marginRedX + 20) || (state.blueSquareY > 610.0 && state.blueSquareX == state.marginBlueX + 30)
    then do
      state <- U.updateState "text" "GAME OVER"
      state <- U.updateState "speedR" 0
      state <- U.updateState "speedB" 0
      state <- U.updateState "score" 0
      pure state
    else
      if state.redSquareY > 610.0
        then do 
          let score1 = score + 1
          state <- U.updateState "score" score1
          squareInitize "red" 
        else 
          if state.blueSquareY > 610.0 
            then do
              let score1 = score + 1
              state <- U.updateState "score" score1
              squareInitize "blue" 
            else
              U.getState
      

moveLeftRed  = do
  state <- U.getState
  state <- U.updateState "marginRedX" 20.0
  let marginRedW = getMargin state.marginRedX 600.0 20.0 20.0
  U.updateState "marginRed" marginRedW

moveRightRed  = do
  state <- U.getState
  state <- U.updateState "marginRedX" 180.0
  let marginRedW = getMargin state.marginRedX 600.0 20.0 20.0
  U.updateState "marginRed" marginRedW

moveLeftBlue  = do
  state <- U.getState
  state <- U.updateState "marginBlueX" 320.0
  let marginBlueW = getMargin state.marginBlueX 600.0 20.0 20.0
  U.updateState "marginBlue" marginBlueW

moveRightBlue  = do
  state <- U.getState
  state <- U.updateState "marginBlueX" 480.0
  let marginBlueW = getMargin state.marginBlueX 600.0 20.0 20.0
  U.updateState "marginBlue" marginBlueW


listen = do
  state <- U.getState
  _ <- down `E.subscribe` (\key -> void $ case key of
                              65 -> moveLeftRed
                              68 -> moveRightRed
                              37 -> moveLeftBlue
                              39 -> moveRightBlue
                              _ -> U.getState)


  
  let behavior = eval <$> millisSinceEpoch
  let events = (animationFrame)
  
  U.patch widget behavior events