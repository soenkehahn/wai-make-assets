module Clock.State exposing (..)

import Time exposing (Time)
import Task exposing (Task, perform)
import Debug exposing (log, crash)

type State
  = Empty
  | State Int Time

init : (State, Cmd (Maybe Message))
init = (Empty, ignoreFail Tick Time.now)

ignoreFail : (a -> message) -> Task x a -> Cmd (Maybe message)
ignoreFail f task =
  perform (\ _ -> Nothing) (Just << f) task

type Message
  = Left
  | Right
  | Tick Time

update : Maybe Message -> State -> (State, Cmd (Maybe Message))
update message state =
  (\ new -> (new, Cmd.none)) <| case message of
    Nothing -> state
    Just m ->
      let result = u m state
      in
      --   Debug.log (toString result)
         result

u : Message -> State -> State
u message state =
  case (message, state) of
    (Left, State i t) -> State (i - 1) t
    (Right, State i t) -> State (i + 1) t
    (Tick newTime, Empty) -> State 0 newTime
    (Tick newTime, State i _) -> State i newTime
    (Left, Empty) -> state
    (Right, Empty) -> state
