module Clock.View exposing (..)

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

import           Clock.State exposing (..)

view : State -> Html.Html (Maybe Message)
view state =
  embed 200 200 (viewState state)

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
    mkCircle "#55a" box ++
    mkHand secondHandLen secondAngle "#f00" 1 box ++
    mkHand minuteHandLen minuteAngle "#ddd" 3 box ++
    mkHand hoursHandLen hoursAngle "#aaa" 10 box ++
    []

mkHand : Float -> Float -> String -> Float -> Box -> List (Svg a)
mkHand len angle color width box =
  let
    xEnd = sin angle * len + fst (center box)
    yEnd = 0 - cos angle * len + snd (center box)
  in
    mkLine (center box) (xEnd, yEnd) color width

mkLine : (Float, Float) -> (Float, Float) -> String -> Float -> List (Svg a)
mkLine start end color width =
  let
    attributes =
      x1 (toString (fst start)) :: y1 (toString (snd start)) ::
      x2 (toString (fst end)) :: y2 (toString (snd end)) ::
      stroke color ::
      strokeWidth (toString width) ::
      []
  in [line attributes []]

mkCircle : String -> Box -> List (Svg a)
mkCircle color box =
  let
    (x, y) = center box
    radius = Basics.min (fst (center box)) (snd (center box))
    attributes =
      cx (toString x) :: cy (toString y) ::
      r (toString radius) ::
      fill color ::
      []
  in
    [circle attributes []]

handAngles : Int -> Time -> (Float, Float, Float)
handAngles utcOffset time =
  let seconds = floor (time / 1000)
      secondAngle = tau * toFloat (seconds % 60) / 60
      minutes = floor (toFloat seconds / 60)
      minuteAngle = tau * toFloat (minutes % 60) / 60
      hours = floor (toFloat minutes / 60) + utcOffset
      hoursAngle = tau * toFloat (hours % 12) / 12
  in (secondAngle, minuteAngle, hoursAngle)

tau : Float
tau = 2 * pi