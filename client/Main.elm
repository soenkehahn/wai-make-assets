
import           Html exposing (Html, hr, div)
import           Html.App exposing (..)
import           List exposing (foldl, repeat)

import           Clock.Main exposing (clock)
import           Combine exposing (ProgramDeclaration, combine)

main : Program Never
main = program Clock.Main.clock
