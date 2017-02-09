module Main exposing (..)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Dict
import Dict.Avl as Avl


dictVsAvl : Benchmark
dictVsAvl =
    describe "Dict vs Dict.Avl" []


main : BenchmarkProgram
main =
    program dictVsAvl
