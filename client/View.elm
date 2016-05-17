module View exposing (..)

import Html.App exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Svg exposing (..)
import VirtualDom exposing (..)
import Svg.Attributes exposing (..)
import Keyboard exposing (..)
import Platform.Sub
import Time exposing (Time)

import State exposing (..)

view : State -> Html.Html (Maybe Message)
view state =
  div [attribute "align" "center"] <|
    Html.text "here" ::
    br [] [] ::
    Svg.svg [width "600", height "600", viewBox "0 0 600 600"] (viewState state) ::
    []

viewState : State -> List (Svg a)
viewState state = case state of
  Empty -> []
  (State i time) ->
    viewI i ++
    viewTime time

viewI : Int -> List (Svg a)
viewI i =
  rect [x "0", y "0", width "10000", height "10000", fill "#000"] [] ::
  rect [x "10", y "10", width "100", height (toString (10 * i)), fill "#88f"] [] ::
  []

viewTime : Time -> List (Svg a)
viewTime time =
  let
    secondAngle = handAngles time
    xEnd = sin secondAngle * 100 + 100
    yEnd = 0 - cos secondAngle * 100 + 100
  in
    line
      (x1 "100" :: y1 "100" ::
       x2 (toString xEnd) :: y2 (toString yEnd) ::
       stroke "#ff0" ::
       [])
      [] ::
  Debug.log (toString <| handAngles time) []

handAngles : Time -> Float
handAngles time =
  let seconds = mod (toFloat <| round (time / 1000)) 60
      secondAngle = tau * seconds / 60
  in Debug.log (toString seconds) secondAngle

tau : Float
tau = 2 * pi

mod : Float -> Float -> Float
mod a b =
  if a >= b
    then mod (a - b) b
    else a
