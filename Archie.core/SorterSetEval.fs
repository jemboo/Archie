namespace Archie.Base
open System
open SorterParts

module SorterSetEval =

    let fitnessInt (offset:float) (value:int) =
        let fv = (float value)
        match fv with
        | v when v > offset -> SorterFitness.create "" (1.0 / (v - offset)) |> Result.ExtractOrThrow
        | v -> SorterFitness.create "" (1.0 / (offset + 1.0 - v)) |> Result.ExtractOrThrow