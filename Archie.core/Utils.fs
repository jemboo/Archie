namespace Archie.Base
open System.Collections.Generic
open Microsoft.FSharp.Core
open System
open System.IO

module ParseUtils =
    let MakeInt32 (str:string) =
        let mutable oot = 0
        let res = Int32.TryParse(str, &oot)
        if res then
            oot |> Ok
        else
            sprintf "Not an int: %s" str |> Error

    let MakeFloat (str:string) =
        let mutable oot = 0.0
        let res = Double.TryParse(str, &oot)
        if res then
            oot |> Ok
        else
            sprintf "Not a float: %s" str |> Error

    let StringToOneInt (str:string) =
        let pcs = str.Split(" ", StringSplitOptions.RemoveEmptyEntries)
        if pcs.Length <> 1 then
            sprintf "1 param expected, not %d" pcs.Length |> Error
        else
            MakeInt32 pcs.[0]

    let StringToOneFloat (str:string) =
        let pcs = str.Split(" ", StringSplitOptions.RemoveEmptyEntries)
        if pcs.Length <> 1 then
            sprintf "1 param expected, not %d" pcs.Length |> Error
        else
            MakeFloat pcs.[0]


module Utils =

//// Converts seq of key - value pairs to mutable Dictionary
//    let ofSeq (src:seq<'a * 'b>) = 
//       let d = new Dictionary<'a, 'b>()
//       for (k,v) in src do
//           d.Add(k,v)
//       d

    let logFile path res (append:bool) =
        use sw =
            new StreamWriter(path, append)
        fprintfn sw "%s" res

    let toMap (keyMaker:'a->'b) (src:seq<'a>) =
        src |> Seq.map(fun v -> (keyMaker v), v)
            |> Map.ofSeq

    let mapSubset (m:Map<'a,'v>) (keys:seq<'a>) = 
        keys |> Seq.map(fun k-> k, (m.[k]))
             |> Map.ofSeq

    let join (m:Map<'a,'v>) (j:seq<'a*'w>) =
        seq { for kv in j do
                if (m.ContainsKey (fst kv)) then
                        yield (fst kv, (m.[fst kv], snd kv)) }


    // get a seq of key-value pairs for easy iteration with for (k,v) in d do...
    let pairs (d:Dictionary<'a, 'b>) =
       seq {
           for kv in d do
               yield (kv.Key, kv.Value)
       }

    let printIntArray (d:int[]) =
        let sb = new System.Text.StringBuilder()
        d |> Seq.map(fun i -> sb.Append(sprintf "%d%s" i Environment.NewLine))
          |> Seq.toArray
          |> ignore
        sb.ToString()

    let printArray (d:'a[]) =
        let sb = new System.Text.StringBuilder()
        d |> Seq.map(fun i -> sb.Append(sprintf "%A%s" i Environment.NewLine))
          |> Seq.toArray
          |> ignore
        sb.ToString()

    let printArrayf f (d:'a[]) =
        let sb = new System.Text.StringBuilder()
        d |> Seq.map(fun i -> sb.Append(sprintf "%A%s" (f i) Environment.NewLine))
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
