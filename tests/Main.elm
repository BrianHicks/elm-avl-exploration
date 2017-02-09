port module Main exposing (..)

import Dict.AvlTests
import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)


main : TestProgram
main =
    run emit Dict.AvlTests.all


port emit : ( String, Value ) -> Cmd msg
