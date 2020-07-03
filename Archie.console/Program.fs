// Learn more about F# at http://fsharp.org

open System
open Archie.core
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open System.Security.Cryptography

[<MemoryDiagnoser>]
type Md5VsSha256() =
    let N = 1000000
    let rando = new Random(42)
    let data = Array.zeroCreate N
    let res = rando.NextBytes(data)
    let sha256 = SHA256.Create();
    let md5 = MD5.Create()

    member this.GetData =
        data

    [<Benchmark>]
    member this.Sha256() =
        sha256.ComputeHash(data)

    [<Benchmark>]
    member this.Md5() =
        md5.ComputeHash(data)


[<EntryPoint>]
let main argv =
    let summary = BenchmarkRunner.Run<Md5VsSha256>()
    printfn "%A" summary
    Console.ReadKey() |> ignore
    0 // return an integer exit code
