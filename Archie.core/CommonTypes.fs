namespace Archie.Base
open System

type String50 = private String50 of string
type Degree = private Degree of int
type EntityId = private EntityId of Guid
type GenerationNumber = private GenerationNumber of int
type InitialConditionCount = private InitialConditionCount of int
type JsonString = private JsonString of string
type MutationRate = private MutationRate of float
type PoolFraction = private PoolFraction of float
type PoolGenCount = private PoolGenCount of int
type PoolMemberRank = private PoolMemberRank of int
type RandomSeed = private RandomSeed of int
type ReplicaCount = private ReplicaCount of int
type ReportingFrequency = private ReportingFrequency of int
type RngType = | Lcg | Net
type RngGen = {rngType:RngType; seed:RandomSeed}
type SortableCount = private SortableCount of int
type SorterCount = private SorterCount of int
type StageCount = private StageCount of int
type SorterFitness = private SorterFitness of float
type SorterFitnessParam = private SorterFitnessParam of float
type SwitchCount = private SwitchCount of int
type UseEagerProc = private UseEagerProc of bool
type UseParallel = private UseParallel of bool

type IRando =
    abstract member Count: int
    abstract member Seed : RandomSeed
    abstract member NextUInt : uint32
    abstract member NextPositiveInt: int32
    abstract member NextULong : uint64
    abstract member NextFloat : float
    abstract member RngType : RngType

type SwitchOrStage = | Switch | Stage

type MutationType = | Switch of MutationRate 
                    | Stage of MutationRate

type SorterLength = | Switch of SwitchCount
                    | Stage of StageCount


type SorterFitnessFunc =
| Switch of SorterFitnessParam
| Stage of SorterFitnessParam


module Degree =
    let value (Degree v) = v
    let create fieldName v = 
        ConstrainedType.createInt fieldName Degree 1 1000 v
    let within (b:Degree) v =
        (v >= 0) && (v < (value b))
    let fromInt v = create "" v |> Result.ExtractOrThrow
    
module EntityId =
    let value (EntityId v) = v
    let create id = Ok (EntityId id)

module InitialConditionCount =
    let value (InitialConditionCount v) = v
    let create fieldName v = 
        ConstrainedType.createInt fieldName InitialConditionCount 1 100000 v
    let fromInt v = create "" v |> Result.ExtractOrThrow

module GenerationNumber =
    let value (GenerationNumber v) = v
    let create fieldName v = 
        ConstrainedType.createInt fieldName GenerationNumber 0 100000000 v
    let fromInt v = create "" v |> Result.ExtractOrThrow

module JsonString =
    let value (JsonString str) = str
    let create fieldName str = 
        ConstrainedType.createString fieldName JsonString 100000000 str
    let createOption fieldName str = 
        ConstrainedType.createStringOption fieldName JsonString 50 str

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

module PoolGenCount =
    let value (PoolGenCount v) = v
    let create fieldName v = 
        ConstrainedType.createInt fieldName PoolGenCount 1 1000000000 v
    let fromInt v = create "" v |> Result.ExtractOrThrow

module PoolMemberRank =
    let value (PoolMemberRank v) = v
    let create fieldName v = 
        ConstrainedType.createInt fieldName PoolMemberRank 1 100000 v
    let fromInt v = create "" v |> Result.ExtractOrThrow
    let repStr v = match v with
                    |Some r -> sprintf "%d" (value r)
                    |None -> ""

module RandomSeed =
    let value (RandomSeed seed) = seed
    let create fieldName (seed:int) =
        let mSeed = Math.Abs(seed) % 2147483647
        ConstrainedType.createInt fieldName RandomSeed 1 2147483647 mSeed
    let fromInt v = create "" v |> Result.ExtractOrThrow

module ReplicaCount =
    let value (ReplicaCount count) = count
    let create fieldName (count:int) =
        ConstrainedType.createInt fieldName ReplicaCount 1 10000 count
    let fromInt v = create "" v |> Result.ExtractOrThrow

module ReportingFrequency =
    let value (ReportingFrequency freq) = freq
    let create fieldName (freq:int) =
        ConstrainedType.createInt fieldName ReportingFrequency 1 10000 freq
    let fromInt v = create "" v |> Result.ExtractOrThrow

module RngGen =
    let createLcg (seed:int) =
        let rnd = (RandomSeed.create "" seed) |> Result.ExtractOrThrow
        {rngType=RngType.Lcg; seed=rnd}

    let createNet (seed:int) =
        let rnd = (RandomSeed.create "" seed) |> Result.ExtractOrThrow
        {rngType=RngType.Net; seed=rnd}

module RngType =
    let toDto (rngt: RngType) =
        match rngt with
        | Lcg -> "Lcg"
        | Net -> "Net"
    let create str =
        match str with
        | "Lcg" -> RngType.Lcg |> Ok
        | "Net" -> RngType.Net |> Ok
        | _ -> Error (sprintf "no match for RngType: %s" str)

module String50 =
    let value (String50 str) = str
    let create fieldName str = 
        ConstrainedType.createString fieldName String50 50 str
    let createOption fieldName str = 
        ConstrainedType.createStringOption fieldName String50 50 str

module SorterCount =
    let value (SorterCount v) = v
    let create fieldName v = 
        ConstrainedType.createInt fieldName SorterCount 1 100000 v
    let fromInt v = create "" v |> Result.ExtractOrThrow

module SortableCount =
    let value (SortableCount v) = v
    let create fieldName v = 
        ConstrainedType.createInt fieldName SortableCount 0 100000000 v
    let fromInt v = create "" v |> Result.ExtractOrThrow
    let repStr v = match v with
                          |Some r -> sprintf "%d" (value r)
                          |None -> ""

module SorterFitness =
    let value (SorterFitness v) = v
    let create fieldName v = 
        ConstrainedType.createFloat fieldName SorterFitness -100000.0 10000.0 v
    let fromFloat v = create "" v |> Result.ExtractOrThrow
    let repStr v = match v with
                          |Some r -> sprintf "%f" (value r)
                          |None -> ""

module SorterFitnessParam =
    let value (SorterFitnessParam v) = v
    let create fieldName v = 
        ConstrainedType.createFloat fieldName SorterFitnessParam 1.0 1000000.0 v
    let fromFloat v = create "" v |> Result.ExtractOrThrow

module SwitchCount =
    let value (SwitchCount v) = v
    let create fieldName v = 
        ConstrainedType.createInt fieldName SwitchCount 0 10000 v
    let fromInt v = create "" v |> Result.ExtractOrThrow

module StageCount =
    let value (StageCount v) = v
    let create fieldName v = 
        ConstrainedType.createInt fieldName StageCount 0 1000 v
    let ToSwitchCount (degree:Degree) (stageCount:StageCount) =
        SwitchCount.create "" ((Degree.value degree) * (value stageCount) / 2)
    let fromInt v = create "" v |> Result.ExtractOrThrow

module UseParallel = 
    let create (useParallel:bool) =
        UseParallel useParallel
    let value (UseParallel v) = v

module UseEagerProc = 
    let create (useEagerProc:bool) =
        UseEagerProc useEagerProc
    let value (UseEagerProc v) = v
    
module SorterLength =

    let degreeToSwitchCount (degree:Degree) =
        let d = (Degree.value degree)
        let ct = match d with
                    | 10 -> 800
                    | 12 -> 1000
                    | 14 -> 1200
                    | 16 -> 1600
                    | 18 -> 2000
                    | 20 -> 2200
                    | 22 -> 2600
                    | 24 -> 3000
                    | _ -> 0
        let wc = SwitchCount.create "" ct |> Result.ExtractOrThrow
        SorterLength.Switch wc


    let degreeToStageCount (degree:Degree) =
        let d = (Degree.value degree)
        let ct = match d with
                    | 10 -> 160
                    | 12 -> 160
                    | 14 -> 160
                    | 16 -> 200
                    | 18 -> 200
                    | 20 -> 200
                    | 22 -> 220
                    | 24 -> 220
                    | _ -> 0
        let tc = StageCount.create "" ct |> Result.ExtractOrThrow
        SorterLength.Stage tc


    let to999Sucessful (degree:Degree) (wOrT:SwitchOrStage) =
        match wOrT with
        | SwitchOrStage.Switch -> (degreeToSwitchCount degree) |> Ok
        | SwitchOrStage.Stage -> (degreeToStageCount degree) |> Ok
        | _ -> Error (sprintf "not handled: %A" wOrT)


    let makeSwitchCount switchCount =
        result {
            let! wc = (SwitchCount.create "" switchCount)
            return SorterLength.Switch wc
        }


    let makeStageCount stageCount =
        result {
            let! tc = (StageCount.create "" stageCount)
            return SorterLength.Stage  tc
        }
       

