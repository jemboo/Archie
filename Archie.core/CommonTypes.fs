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
type PoolCount = private PoolCount of int
type PoolGenCount = private PoolGenCount of int
type PoolMemberRank = private PoolMemberRank of int
type RandomSeed = private RandomSeed of int
type ReplicaCount = private ReplicaCount of int
type ReportingFrequency = private ReportingFrequency of int
type RngType = | Lcg | Net
type RngGen = {rngType:RngType; seed:RandomSeed}
type RunCount = private RunCount of int
type SortableCount = private SortableCount of int
type SorterCount = private SorterCount of int
type StageCount = private StageCount of int
type SorterFitness = private SorterFitness of float
type SwitchCount = private SwitchCount of int
type SwitchFrequency = private SwitchFrequency of float
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

type SorterMutationType = | Switch of MutationRate 
                          | Stage of MutationRate

type SorterLength = | Switch of SwitchCount
                    | Stage of StageCount


module Degree =
    let value (Degree v) = v
    let create fieldName v = 
        ConstrainedType.createInt fieldName Degree 1 1000 v
    let within (b:Degree) v =
        (v >= 0) && (v < (value b))
    let fromInt v = create "" v |> Result.ExtractOrThrow
    let fromKey (m:Map<'a, obj>) (key:'a) =
        result {
            let! gv = CollectionUtils.readMap m key
            return! create "" (gv:?>int)
        }

module EntityId =
    let value (EntityId v) = v
    let create id = Ok (EntityId id)

module InitialConditionCount =
    let value (InitialConditionCount v) = v
    let create fieldName v = 
        ConstrainedType.createInt fieldName InitialConditionCount 1 100000 v
    let fromInt v = create "" v |> Result.ExtractOrThrow
    let fromKey (m:Map<'a, obj>) (key:'a) =
        result {
            let! gv = CollectionUtils.readMap m key
            return! create "" (gv:?>int)
        }

module GenerationNumber =
    let value (GenerationNumber v) = v
    let create fieldName v = 
        ConstrainedType.createInt fieldName GenerationNumber 0 100000000 v
    let fromInt v = create "" v |> Result.ExtractOrThrow
    let increment gen = fromInt ((value gen) + 1)
    let fromKey (m:Map<'a, obj>) (key:'a) =
        result {
            let! gv = CollectionUtils.readMap m key
            return! create "" (gv:?>int)
        }

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
    let fromKey (m:Map<'a, obj>) (key:'a) =
        result {
            let! gv = CollectionUtils.readMap m key
            return! create "" (gv:?>float)
        }

module SorterMutationType =
    let StrF (mt:SorterMutationType) =
        match mt with
        | SorterMutationType.Switch mr -> sprintf "w%.3f" (MutationRate.value mr)
        | SorterMutationType.Stage mr -> sprintf "t%.3f" (MutationRate.value mr)

module PoolFraction =
    let value (PoolFraction v) = v
    let create fieldName v = 
        ConstrainedType.createFloat fieldName PoolFraction 0.0 1.0 v
    let boundedMultiply pf rhs =
        Math.Max((int ((float rhs) * (value pf))), 1)
    let fromFloat v = create "" v |> Result.ExtractOrThrow
    let fromKey (m:Map<'a, obj>) (key:'a) =
        result {
            let! gv = CollectionUtils.readMap m key
            return! create "" (gv:?>float)
        }

module PoolCount =
    let value (PoolCount v) = v
    let create fieldName v = 
        ConstrainedType.createInt fieldName PoolCount 1 10000 v
    let fromInt v = create "" v |> Result.ExtractOrThrow
    let fromKey (m:Map<'a, obj>) (key:'a) =
        result {
            let! gv = CollectionUtils.readMap m key
            return! create "" (gv:?>int)
        }

module PoolGenCount =
    let value (PoolGenCount v) = v
    let create fieldName v = 
        ConstrainedType.createInt fieldName PoolGenCount 1 1000000000 v
    let fromInt v = create "" v |> Result.ExtractOrThrow
    let fromKey (m:Map<'a, obj>) (key:'a) =
        result {
            let! gv = CollectionUtils.readMap m key
            return! create "" (gv:?>int)
        }


module PoolMemberRank =
    let value (PoolMemberRank v) = v
    let create fieldName v = 
        ConstrainedType.createInt fieldName PoolMemberRank 1 100000 v
    let fromInt v = create "" v |> Result.ExtractOrThrow
    let repStr v = match v with
                    |Some r -> sprintf "%d" (value r)
                    |None -> ""
    let fromKey (m:Map<'a, obj>) (key:'a) =
        result {
            let! gv = CollectionUtils.readMap m key
            return! create "" (gv:?>int)
        }

module RandomSeed =
    let value (RandomSeed seed) = seed
    let create fieldName (seed:int) =
        let mSeed = Math.Abs(seed) % 2147483647
        ConstrainedType.createInt fieldName RandomSeed 1 2147483647 mSeed
    let fromInt v = create "" v |> Result.ExtractOrThrow
    let fromKey (m:Map<'a, obj>) (key:'a) =
        result {
            let! gv = CollectionUtils.readMap m key
            return! create "" (gv:?>int)
        }

module ReplicaCount =
    let value (ReplicaCount count) = count
    let create fieldName (count:int) =
        ConstrainedType.createInt fieldName ReplicaCount 1 10000 count
    let fromInt v = create "" v |> Result.ExtractOrThrow
    let fromKey (m:Map<'a, obj>) (key:'a) =
        result {
            let! gv = CollectionUtils.readMap m key
            return! create "" (gv:?>int)
        }

module ReportingFrequency =
    let value (ReportingFrequency freq) = freq
    let create fieldName (freq:int) =
        ConstrainedType.createInt fieldName ReportingFrequency 1 10000 freq
    let fromInt v = create "" v |> Result.ExtractOrThrow
    let fromKey (m:Map<'a, obj>) (key:'a) =
        result {
            let! gv = CollectionUtils.readMap m key
            return! create "" (gv:?>int)
        }

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

module RunCount =
    let value (RunCount v) = v
    let create fieldName v = 
        ConstrainedType.createInt fieldName RunCount 1 1000000 v
    let fromInt v = create "" v |> Result.ExtractOrThrow
    let fromKey (m:Map<'a, obj>) (key:'a) =
        result {
            let! gv = CollectionUtils.readMap m key
            return! create "" (gv:?>int)
        }


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
    let fromKey (m:Map<'a, obj>) (key:'a) =
        result {
            let! gv = CollectionUtils.readMap m key
            return! create "" (gv:?>int)
        }

module SortableCount =
    let value (SortableCount v) = v
    let create fieldName v = 
        ConstrainedType.createInt fieldName SortableCount 0 100000000 v
    let fromInt v = create "" v |> Result.ExtractOrThrow
    let repStr v = match v with
                          |Some r -> sprintf "%d" (value r)
                          |None -> ""
    let fromKey (m:Map<'a, obj>) (key:'a) =
        result {
            let! gv = CollectionUtils.readMap m key
            return! create "" (gv:?>int)
        }

module SorterFitness =
    let value (SorterFitness v) = v
    let create fieldName v = 
        ConstrainedType.createFloat fieldName SorterFitness -100000.0 10000.0 v
    let fromFloat v = create "" v |> Result.ExtractOrThrow
    let repStr v = match v with
                          |Some r -> sprintf "%.4f" (value r)
                          |None -> ""
    let fromKey (m:Map<'a, obj>) (key:'a) =
        result {
            let! gv = CollectionUtils.readMap m key
            return! create "" (gv:?>float)
        }

module SwitchCount =
    let value (SwitchCount v) = v
    let create fieldName v = 
        ConstrainedType.createInt fieldName SwitchCount 0 10000 v
    let fromInt v = create "" v |> Result.ExtractOrThrow
    let fromKey (m:Map<'a, obj>) (key:'a) =
        result {
            let! gv = CollectionUtils.readMap m key
            return! create "" (gv:?>int)
        }

module SwitchFrequency =
    let value (SwitchFrequency v) = v
    let create fieldName v = 
        ConstrainedType.createFloat fieldName SwitchFrequency 0.0 1.0 v
    let fromFloat v = create "" v |> Result.ExtractOrThrow
    let fromKey (m:Map<'a, obj>) (key:'a) =
        result {
            let! gv = CollectionUtils.readMap m key
            return! create "" (gv :?> float)
        }


module StageCount =
    let value (StageCount v) = v
    let create fieldName v = 
        ConstrainedType.createInt fieldName StageCount 0 1000 v
    let ToSwitchCount (degree:Degree) (stageCount:StageCount) =
        SwitchCount.create "" ((Degree.value degree) * (value stageCount) / 2)
    let fromInt v = create "" v |> Result.ExtractOrThrow
    let fromKey (m:Map<'a, obj>) (key:'a) =
        result {
            let! gv = CollectionUtils.readMap m key
            return! create "" (gv:?>int)
        }

module UseParallel = 
    let create (useParallel:bool) =
        UseParallel useParallel
    let value (UseParallel v) = v
    let fromKey (m:Map<'a, obj>) (key:'a) =
        result {
            let! gv = CollectionUtils.readMap m key
            return create (gv:?>bool)
        }

module UseEagerProc = 
    let create (useEagerProc:bool) =
        UseEagerProc useEagerProc
    let value (UseEagerProc v) = v
    let fromKey (m:Map<'a, obj>) (key:'a) =
        result {
            let! gv = CollectionUtils.readMap m key
            return create (gv:?>bool)
        }

module SorterLength =

    let makeSwitchCountR switchCount =
        result {
            let! wc = (SwitchCount.create "" switchCount)
            return SorterLength.Switch wc
        }

    let makeStageCountR stageCount =
        result {
            let! tc = (StageCount.create "" stageCount)
            return SorterLength.Stage  tc
        }
    
    let makeStageCount stageCount =
        SorterLength.Stage (StageCount.fromInt stageCount)

    let makeSwitchCount switchCount =
        SorterLength.Switch (SwitchCount.fromInt switchCount)


    let Add (lhs:SorterLength) (rhs:SorterLength) =
            match lhs with
                | SorterLength.Switch (SwitchCount wCtL) -> 
                        match rhs with
                        | SorterLength.Switch (SwitchCount wCtR) -> makeSwitchCount (wCtL + wCtR) |> Result.Ok
                        | SorterLength.Stage _ -> Error "cant add SwitchCount and StageCount"
                | SorterLength.Stage (StageCount tCtL) -> 
                        match rhs with
                        | SorterLength.Switch _ -> Error "cant add SwitchCount and StageCount"
                        | SorterLength.Stage (StageCount tCtR) -> makeStageCount (tCtL + tCtR) |> Result.Ok


    let degreeToRecordSwitchCount (degree:Degree) =
        let d = (Degree.value degree)
        let ct = match d with
                    | 4 -> 5
                    | 5 -> 9
                    | 6 -> 12
                    | 7 -> 16
                    | 8 -> 19
                    | 9 -> 25
                    | 10 -> 29
                    | 11 -> 35
                    | 12 -> 39
                    | 13 -> 45
                    | 14 -> 51
                    | 15 -> 56
                    | 16 -> 60
                    | 17 -> 71
                    | 18 -> 77
                    | 19 -> 85
                    | 20 -> 91
                    | 21 -> 100
                    | 22 -> 107
                    | 23 -> 115
                    | 24 -> 120
                    | 25 -> 132
                    | 26 -> 139
                    | 27 -> 150
                    | 28 -> 155
                    | 29 -> 165
                    | 30 -> 172
                    | 31 -> 180
                    | 32 -> 185
                    | _ -> 0
        let wc = SwitchCount.create "" ct |> Result.ExtractOrThrow
        SorterLength.Switch wc


    let degreeTo999SwitchCount (degree:Degree) =
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


    let degreeToRecordStageCount (degree:Degree) =
        let d = (Degree.value degree)
        let ct = match d with
                    | 4 ->  3
                    | 5 ->  5
                    | 6 ->  5
                    | 7 ->  6
                    | 8 ->  6
                    | 9 ->  7
                    | 10 -> 7
                    | 11 -> 8
                    | 12 -> 8
                    | 13 -> 9
                    | 14 -> 9
                    | 15 -> 9
                    | 16 -> 9
                    | 17 -> 10
                    | 18 -> 11
                    | 19 -> 11
                    | 20 -> 11
                    | 21 -> 12
                    | 22 -> 12
                    | 23 -> 12
                    | 24 -> 12
                    | 25 -> 13
                    | 26 -> 13
                    | 27 -> 14
                    | 28 -> 14
                    | 29 -> 14
                    | 30 -> 14
                    | 31 -> 14
                    | 32 -> 14
                    | _ -> 0
        let tc = StageCount.create "" ct |> Result.ExtractOrThrow
        SorterLength.Stage tc


    let degreeTo999StageCount (degree:Degree) =
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
        | SwitchOrStage.Switch -> (degreeTo999SwitchCount degree)
        | SwitchOrStage.Stage -> (degreeTo999StageCount degree)

    let toRecordSorterLength (degree:Degree) (wOrT:SwitchOrStage) =
        match wOrT with
        | SwitchOrStage.Switch -> (degreeToRecordSwitchCount degree)
        | SwitchOrStage.Stage -> (degreeToRecordStageCount degree)

    let toRecordSorterLengthPlus(degree:Degree) (extraLength:SorterLength) =
        match extraLength with
        | SorterLength.Switch wCt -> (toRecordSorterLength degree SwitchOrStage.Switch) |> Add extraLength |> Result.ExtractOrThrow
        | SorterLength.Stage wCt -> (toRecordSorterLength degree SwitchOrStage.Stage) |> Add extraLength |> Result.ExtractOrThrow

       

