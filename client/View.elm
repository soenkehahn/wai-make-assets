module View exposing (..)

import           Html exposing (..)
import           Html.App exposing (..)
import           Html.Events exposing (..)
import           Keyboard exposing (..)
import           List exposing (map)
import           Platform.Sub
import           String exposing (join)
import           Svg exposing (..)
import           Svg.Attributes exposing (..)
import           Time exposing (Time)
import           VirtualDom exposing (..)

import           State exposing (..)

view : State -> Html.Html (Maybe Message)
view state =
  div [attribute "align" "center"] <|
    Html.text "here" ::
    br [] [] ::
    embed 400 400 (viewState state) ::
    []

type Box
  = Box Int Int Int Int

center : Box -> (Float, Float)
center (Box x1 y1 x2 y2) =
  (toFloat (x2 - x1) / 2, toFloat (y2 - y1) / 2)

embed : Int -> Int -> (Box -> List (Svg a)) -> Html a
embed w h contents =
  let
    attributes =
      width (toString w) :: height (toString h) ::
      viewBox (join " " (List.map toString [0, 0, w, h])) ::
      []
    box = Box 0 0 w h
    background =
      rect [x "0", y "0", width (toString w), height (toString h), fill "#000"] []
  in Svg.svg attributes (background :: contents box)

viewState : State -> Box -> List (Svg a)
viewState state box = case state of
  Empty -> []
  (State i time) ->
    viewI i ++
    viewTime time box

viewI : Int -> List (Svg a)
viewI i =
  rect [x "10", y "10", width "100", height (toString (10 * i)), fill "#88f"] [] ::
  []

viewTime : Time -> Box -> List (Svg a)
viewTime time box =
  let
    secondHandLen = Basics.min (fst (center box)) (snd (center box))
    minuteHandLen = secondHandLen * 0.8
    hoursHandLen = secondHandLen * 0.4
    (secondAngle, minuteAngle, hoursAngle) = handAngles 8 time
  in
    mkHand secondHandLen secondAngle "#f00" box ++
    mkHand minuteHandLen minuteAngle "#fff" box ++
    mkHand hoursHandLen hoursAngle "#fff" box

mkHand : Float -> Float -> String -> Box -> List (Svg a)
mkHand len angle color box =
  let
    xEnd = sin angle * len + fst (center box)
    yEnd = 0 - cos angle * len + snd (center box)
  in
    mkLine (center box) (xEnd, yEnd) color

mkLine : (Float, Float) -> (Float, Float) -> String -> List (Svg a)
mkLine start end color =
  let
    attributes =
      x1 (toString (fst start)) :: y1 (toString (snd start)) ::
      x2 (toString (fst end)) :: y2 (toString (snd end)) ::
      stroke color ::
      []
  in [line attributes []]

handAngles : Int -> Time -> (Float, Float, Float)
handAngles utcOffset time =
  let seconds = floor (time / 1000)
      secondAngle = tau * toFloat (seconds % 60) / 60
      minutes = floor (toFloat seconds / 60)
      minuteAngle = tau * toFloat (Debug.log "min" (minutes % 60)) / 60
      hours = floor (toFloat minutes / 60) + utcOffset
      hoursAngle = tau * toFloat (Debug.log "hours" (hours % 12)) / 12
  in (secondAngle, minuteAngle, hoursAngle)

tau : Float
tau = 2 * pi
