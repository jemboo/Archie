namespace Archie.Base
open System.Collections.Generic
open Microsoft.FSharp.Core

module Utils =

// Converts seq of key - value pairs to mutable Dictionary
    let ofSeq (src:seq<'a * 'b>) = 
       let d = new Dictionary<'a, 'b>()
       for (k,v) in src do
           d.Add(k,v)
       d

    // get a seq of key-value pairs for easy iteration with for (k,v) in d do...
    let pairs (d:Dictionary<'a, 'b>) =
       seq {
           for kv in d do
               yield (kv.Key, kv.Value)
       }

    let printIntArray (d:int[]) =
        let sb = new System.Text.StringBuilder()
        d |> Seq.map(fun i -> sb.Append(sprintf "%d " i))
          |> Seq.toArray
          |> ignore
        sb.ToString()

    let printTupes (d:seq<string*'A>) =
       let sb = new System.Text.StringBuilder()
       d |> Seq.map(fun i -> sb.Append(sprintf "%s=%A, " (fst i) (snd i)))
         |> Seq.toArray
         |> ignore
       sb.ToString()

    let histogram<'d,'r when 'r:comparison> (keymaker:'d->'r) (qua:seq<'d>) =
        qua
        |> Seq.fold (fun acc fv ->
                let kk = keymaker fv
                if Map.containsKey kk acc
                then Map.add kk (acc.[kk] + 1) acc
                else Map.add kk 1 acc
            ) Map.empty
