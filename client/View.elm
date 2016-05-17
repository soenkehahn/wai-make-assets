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
    embed 600 600 (viewState state) ::
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
    secondAngle = handAngles time
    xEnd = sin secondAngle * secondHandLen + fst (center box)
    yEnd = 0 - cos secondAngle * secondHandLen + snd (center box)
  in
    Debug.log "time" <| mkLine (center box) (xEnd, yEnd)

mkLine : (Float, Float) -> (Float, Float) -> List (Svg a)
mkLine start end =
  let
    attributes =
      x1 (toString (fst start)) :: y1 (toString (snd start)) ::
      x2 (toString (fst end)) :: y2 (toString (snd end)) ::
      stroke "#ff0" ::
      []
  in [line attributes []]

handAngles : Time -> Float
handAngles time =
  let seconds = Debug.log "seconds" <| toFloat (round (time / 1000) % 60)
      secondAngle = tau * seconds / 60
  in secondAngle

tau : Float
tau = 2 * pi
