namespace Archie.console
open System
open Archie.Base
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open System.Security.Cryptography
open Archie.Base.Sorting
open Archie.Base.Combinatorics_Types

//module Benchmarks

//[<MemoryDiagnoser>]
type Md5VsSha256() =
    let N = 10000
    let rando = new Random(42)
    let data = Array.zeroCreate N
    let res = rando.NextBytes(data)
    let sha256 = SHA256.Create();
    let md5 = MD5.Create()

    member this.GetData =
        data

    [<Benchmark(Baseline = true)>]
    member this.Sha256() =
        sha256.ComputeHash(data)

    [<Benchmark>]
    member this.Md5() =
        md5.ComputeHash(data)

//[<MemoryDiagnoser>]
type RandoBench() =
    let seed = RandomSeed.create "" 424 |> Result.toOption
    let seed2 = RandomSeed.create "" 42 |> Result.toOption
    let randoNet = new RandomNet(seed.Value)
    let randoLcg = new RandomLcg(seed.Value)
    let randoNet2 = new RandomNet(seed2.Value)
    let randoLcg2 = new RandomLcg(seed2.Value)

    [<Benchmark(Baseline = true)>]
    member this.Net() =
        Rando.NextGuid randoNet

    [<Benchmark>]
    member this.Lcg() =
        Rando.NextGuid randoLcg

    [<Benchmark>]
    member this.Net2() =
        Rando.NextGuid2 randoNet randoNet2

    [<Benchmark>]
    member this.Lcg2() =
        Rando.NextGuid2 randoLcg randoLcg2
