
module Clock.Main exposing (State, main, clock)

import Html.App exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Svg exposing (..)
import VirtualDom exposing (..)
import Svg.Attributes exposing (..)
import Keyboard exposing (..)
import Platform.Sub
import Time

import Clock.State exposing (..)
import Clock.View as View exposing (..)

type alias State = Clock.State.State

main : Program Never
main = Html.App.program clock

clock = {
  init = init,
  subscriptions = subscriptions,
  update = update,
  view = View.view
  }

subscriptions : State -> Sub (Maybe Message)
subscriptions _ = Platform.Sub.batch <|
  keys ::
  Platform.Sub.map Just seconds ::
  []

keys : Sub (Maybe Message)
keys =
  downs <| \ code -> case code of
    37 -> Just Left
    39 -> Just Right
    c -> Debug.log (toString c) Nothing

seconds : Sub Message
seconds = Time.every 1000 Tick
