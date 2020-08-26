﻿namespace Archie.Base
open System

module ParseUtils =
    let MakeInt32 (str:string) =
        let mutable oot = 0
        let res = Int32.TryParse(str, &oot)
        if res then
            oot |> Ok
        else
            sprintf "Not an int: %s" str |> Error

type String50 = private String50 of string
type Degree = private Degree of int
type GenerationCount = private GenerationCount of int
type MutationRate = private MutationRate of float
type PoolFraction = private PoolFraction of float
type RandomSeed = private RandomSeed of int
type ReplicaCount = private ReplicaCount of int
type SorterCount = private SorterCount of int
type SwitchCount = private SwitchCount of int
type StageCount = private StageCount of int
type SorterFitness = private SorterFitness of float
type SorterFitnessParam = private SorterFitnessParam of float
type SortableCount = private SortableCount of int
type EntityId = private EntityId of Guid
type JsonString = private JsonString of string
type RngType = | Lcg | Net
type IRando =
    abstract member Count: int
    abstract member Seed : RandomSeed
    abstract member NextUInt : uint32
    abstract member NextPositiveInt: int32
    abstract member NextULong : uint64
    abstract member NextFloat : float
    abstract member RngType : RngType

type MutationType = | Switch of MutationRate | Stage of MutationRate
type RandSorterGeneration = | Switch of SwitchCount | Stage of StageCount
type RngGen = {rngType:RngType; seed:RandomSeed}

type SorterFitnessFunc =
| Switch of SorterFitnessParam
| Stage of SorterFitnessParam

module String50 =
    let value (String50 str) = str
    let create fieldName str = 
        ConstrainedType.createString fieldName String50 50 str
    let createOption fieldName str = 
        ConstrainedType.createStringOption fieldName String50 50 str

module Degree =
    let value (Degree v) = v
    let create fieldName v = 
        ConstrainedType.createInt fieldName Degree 1 1000 v
    let within (b:Degree) v =
        (v >= 0) && (v < (value b))
    let fromInt v = create "" v |> Result.ExtractOrThrow

module GenerationCount =
    let value (GenerationCount v) = v
    let create fieldName v = 
        ConstrainedType.createInt fieldName GenerationCount 1 100000000 v
    let fromInt v = create "" v |> Result.ExtractOrThrow

module MutationRate =
    let value (MutationRate v) = v
    let create fieldName v = 
        ConstrainedType.createFloat fieldName MutationRate 0.0 1.0 v
    let fromFloat v = create "" v |> Result.ExtractOrThrow

module MutationTypeF =
    let StrF (mt:MutationType) =
        match mt with
        | MutationType.Switch mr -> sprintf "w%.3f" (MutationRate.value mr)
        | MutationType.Stage mr -> sprintf "t%.3f" (MutationRate.value mr)

module PoolFraction =
    let value (PoolFraction v) = v
    let create fieldName v = 
        ConstrainedType.createFloat fieldName PoolFraction 0.0 1.0 v
    let boundedMultiply pf rhs =
        Math.Max((int ((float rhs) * (value pf))), 1)
    let fromFloat v = create "" v |> Result.ExtractOrThrow

module RandomSeed =
    let value (RandomSeed seed) = seed
    let create fieldName (seed:int) =
        let mSeed = Math.Abs(seed) % 2147483647
        ConstrainedType.createInt fieldName RandomSeed 1 2147483647 mSeed
    let fromInt v = create "" v |> Result.ExtractOrThrow

module ReplicaCount =
    let value (ReplicaCount seed) = seed
    let create fieldName (seed:int) =
        let mSeed = Math.Abs(seed) % 2147483647
        ConstrainedType.createInt fieldName ReplicaCount 1 10000 mSeed
    let fromInt v = create "" v |> Result.ExtractOrThrow

module SwitchCount =
    let value (SwitchCount v) = v
    let create fieldName v = 
        ConstrainedType.createInt fieldName SwitchCount 1 10000 v
    let fromInt v = create "" v |> Result.ExtractOrThrow

module StageCount =
    let value (StageCount v) = v
    let create fieldName v = 
        ConstrainedType.createInt fieldName StageCount 1 1000 v
    let ToSwitchCount (degree:Degree) (stageCount:StageCount) =
        SwitchCount.create "" ((Degree.value degree) * (value stageCount) / 2)
    let fromInt v = create "" v |> Result.ExtractOrThrow

module SorterCount =
    let value (SorterCount v) = v
    let create fieldName v = 
        ConstrainedType.createInt fieldName SorterCount 1 100000 v
    let fromInt v = create "" v |> Result.ExtractOrThrow

module SortableCount =
    let value (SortableCount v) = v
    let create fieldName v = 
        ConstrainedType.createInt fieldName SortableCount 1 1000 v
    let fromInt v = create "" v |> Result.ExtractOrThrow

module SorterFitness =
    let value (SorterFitness v) = v
    let create fieldName v = 
        ConstrainedType.createFloat fieldName SorterFitness 0.0 1.0 v
    let fromInt v = create "" v |> Result.ExtractOrThrow

module SorterFitnessParam =
    let value (SorterFitnessParam v) = v
    let create fieldName v = 
        ConstrainedType.createFloat fieldName SorterFitnessParam 1.0 1000000.0 v
    let fromFloat v = create "" v |> Result.ExtractOrThrow

module EntityId =
    let value (EntityId v) = v
    let create id = Ok (EntityId id)

module JsonString =
    let value (JsonString str) = str
    let create fieldName str = 
        ConstrainedType.createString fieldName JsonString 100000000 str
    let createOption fieldName str = 
        ConstrainedType.createStringOption fieldName JsonString 50 str

module RngType =
    let toDto (rngt: RngType) =
        match rngt with
        | Lcg -> "Lcg"
        | Net -> "Net"
    let create str =
        match str with
        | "Lcg" -> RngType.Lcg |> Ok
        | "Net" -> RngType.Net |> Ok
        | _ -> Error [|(sprintf "no match for RngType: %s" str)|]


module RandSorterGenerationF =
    
    let toDto (rngt:RandSorterGeneration) =
        match rngt with
        | RandSorterGeneration.Switch ct -> sprintf "Switch %d" (SwitchCount.value ct)
        | RandSorterGeneration.Stage ct -> sprintf "Stage %d" (StageCount.value ct)

    let fromDto (str:string) =
        let finishParse (str:string) ct =
            match str with
            | "Switch" -> RandSorterGeneration.Switch 
                                ((SwitchCount.create "" ct)|> Result.ExtractOrThrow) |> Ok
            | "Stage" -> RandSorterGeneration.Stage 
                                ((StageCount.create "" ct)|> Result.ExtractOrThrow) |> Ok
            | _ -> Error (sprintf "no match for RandSorterGenerationMode: %s" str)

        let doArgs (pcs:string[]) =
            if pcs.Length = 2 then
                result {
                          let! ct = (ParseUtils.MakeInt32 pcs.[1])
                          return! (finishParse pcs.[0] ct)
                       }
              else
                  Error (sprintf "incorrect string: %s" str)
        
        result {
            let pcs = str.Split(" ", StringSplitOptions.RemoveEmptyEntries)
            return! doArgs pcs
        }

    let MakeSwitchCount len = 
        let sc = (SwitchCount.create "" len) |> Result.ExtractOrThrow
        RandSorterGeneration.Switch sc

    let MakeStageCount len = 
        let sc = (StageCount.create "" len) |> Result.ExtractOrThrow
        RandSorterGeneration.Stage  sc