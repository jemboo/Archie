namespace Archie.Base
open System

[<Struct>]
type Switch = {low:int; hi:int}
module Switch =
    let switchMap = 
        [for hi=0 to 64 
            do for low=0 to hi do yield {Switch.low=low; Switch.hi=hi}]

    let getIndex (switch:Switch) =
        (switch.hi * (switch.hi + 1)) / 2 + switch.low

    let zeroSwitches =
        seq { while true do yield {Switch.low=0; Switch.hi=0}}
        
    // produces switches from only the two cycle components of the 
    // permutation
    let fromIntArray (pArray:int[]) =
            seq { for i = 0 to pArray.Length - 1 do
                    let j = pArray.[i]
                    if ((j >= i ) && (i = pArray.[j]) ) then
                            yield {Switch.low=i; Switch.hi=j} }

    let fromPermutation (p:Permutation) =
        fromIntArray (Permutation.arrayValues p)
     
    let fromTwoCyclePerm (p:TwoCyclePerm) =
        fromIntArray (TwoCyclePerm.arrayValues p)
    
    let toString (sw:Switch) =
        sprintf "(%d, %d)" sw.low sw.hi

    let randomSwitchesOfDegree (order:Degree) (rnd:IRando) =
        let mDex = uint32 ((Degree.value order)*(Degree.value order + 1) / 2) 
        seq { while true do 
                    let p = (int (rnd.NextUInt % mDex))
                    yield switchMap.[p] }

    let mutateSwitches (order:Degree) (mutationRate:MutationRate) (rnd:IRando) (switches:seq<Switch>) =
        let mDex = uint32 ((Degree.value order)*(Degree.value order + 1) / 2) 
        let mutateSwitch (switch:Switch) =
            match rnd.NextFloat with
            | k when k < (MutationRate.value mutationRate) -> switchMap.[(int (rnd.NextUInt % mDex))] 
            | _ -> switch
        switches |> Seq.map(fun sw-> mutateSwitch sw)


type SortableIntArray = private SortableIntArray of int[]
module SortableIntArray =
    let create (intArray:int[]) = SortableIntArray intArray
    let Identity (order: int) = create [|0 .. order-1|]
    let value (SortableIntArray p) = p
    let apply f (p:SortableIntArray) = f (value p)

    let copy (sortableIntArray:SortableIntArray) = 
        create (Array.copy (value sortableIntArray))

    let fromRandomPermutation (degree:Degree) (rnd:IRando) =
        create (Combinatorics.randomPermutation rnd (Degree.value degree))

    let fromRandomBits (degree:Degree) (rnd:IRando) =
        create (ZeroOneSequence.Random rnd (Degree.value degree) 0.5 |> Seq.toArray)


type SortableSet = {degree:Degree; baseArray:int[]; count:int}
module SortableSet =
    let create (degree:Degree) (baseArray:int[] ) =
        if baseArray.Length < 0 + (Degree.value degree) then
            Error (sprintf "baseArray length %d is not a multiple of degree: %d:" 
                    baseArray.Length (Degree.value degree))
        else
            let baseCopy = Array.zeroCreate baseArray.Length
            Array.Copy(baseArray, baseCopy, baseArray.Length)
            {degree=degree; baseArray=baseCopy; 
                count=baseCopy.Length / (Degree.value degree) } |> Ok

    let copy (sortableSet:SortableSet) =
        let baseCopy = Array.zeroCreate sortableSet.baseArray.Length
        Array.Copy(sortableSet.baseArray, baseCopy, baseCopy.Length)
        {degree=sortableSet.degree; baseArray=baseCopy;
            count=baseCopy.Length / (Degree.value sortableSet.degree) } |> Ok

    let copy2 (sortableSet:SortableSet) =
        let baseCopy = Array.create sortableSet.baseArray.Length 0
        Array.Copy(sortableSet.baseArray, baseCopy, baseCopy.Length)
        {degree=sortableSet.degree; baseArray=baseCopy;
        count=baseCopy.Length / (Degree.value sortableSet.degree) } |> Ok

    let allBinary (degree:Degree) =
        let baseArray = IntBits.AllBinaryTestCasesArray (Degree.value degree)
                        |> Array.collect(fun a -> a)
        create degree baseArray


type Stage = {switches:Switch list; degree:Degree}
module Stage =
    let createRandom (degree:Degree) (rnd:IRando) =
        let switches = (TwoCyclePerm.makeRandomFullTwoCycle degree rnd )
                        |> Switch.fromTwoCyclePerm
        {switches=switches |> Seq.toList; degree=degree}


    let mergeSwitchesIntoStages (degree:Degree) (switches:seq<Switch>) =
        let mutable stageTracker = Array.init (Degree.value degree) (fun _ -> false)
        let switchesForStage = new ResizeArray<Switch>()
        seq { 
                for sw in switches do
                    if (stageTracker.[sw.hi] || stageTracker.[sw.low] ) then
                        yield { Stage.switches = switchesForStage |> Seq.toList; degree = degree}
                        stageTracker <- Array.init (Degree.value degree) (fun _ -> false)
                        switchesForStage.Clear()
                    stageTracker.[sw.hi] <- true
                    stageTracker.[sw.low] <- true
                    switchesForStage.Add sw
                if switchesForStage.Count > 0 then
                    yield { Stage.switches=switchesForStage |> Seq.toList; 
                            degree = degree}
             }


    let getStageIndexesFromSwitches (degree:Degree) (switches:seq<Switch>) =
        let mutable stageTracker = Array.init (Degree.value degree) (fun _ -> false)
        let mutable curDex = 0
        seq { 
                yield curDex
                for sw in switches do
                    if (stageTracker.[sw.hi] || stageTracker.[sw.low] ) then
                        yield curDex
                        stageTracker <- Array.init (Degree.value degree) (fun _ -> false)
                    stageTracker.[sw.hi] <- true
                    stageTracker.[sw.low] <- true
                    curDex <- curDex + 1
                yield curDex
           }


    let makeRandomStagedSwitchSeq (degree:Degree) (switchFreq:SwitchFrequency) (rnd:IRando) =
        let aa (rnd:IRando)  = 
            (TwoCyclePerm.makeRandomTwoCycle degree rnd (SwitchFrequency.value switchFreq))
                    |> Switch.fromTwoCyclePerm
        seq { while true do yield! (aa rnd) }


    let convertToTwoCycle (stage:Stage) =
        stage.switches |> Seq.map(fun s -> (s.low, s.hi))
                        |> TwoCyclePerm.makeFromTupleSeq stage.degree


    let mutateStage (stage:Stage) (pair:int*int) =
        let tcp = stage |> convertToTwoCycle |> TwoCyclePerm.arrayValues
        let a, b = pair
        let c = tcp.[a]
        let d = tcp.[b]
        if (a=c) && (b=d) then
            tcp.[a] <- b
            tcp.[b] <- a
        elif (a=c) then
            tcp.[a] <- b
            tcp.[b] <- a
            tcp.[d] <- d
        elif (b=d) then
            tcp.[a] <- b
            tcp.[b] <- a
            tcp.[c] <- c
        else
            tcp.[a] <- b
            tcp.[c] <- d
            tcp.[b] <- a
            tcp.[d] <- c
        let sA = Switch.fromIntArray tcp |> Seq.toList
        {switches=sA; degree=stage.degree}

    let randomMutate (rnd:IRando) (mutationRate:MutationRate) (stage:Stage) = 
        match rnd.NextFloat with
            | k when k < (MutationRate.value mutationRate) -> 
                        let tcp = Combinatorics.drawTwoWithoutRep stage.degree rnd
                        mutateStage stage tcp
            | _ -> stage


type Sorter = {degree:Degree; switches:array<Switch>; switchCount:SwitchCount}
module Sorter =
        
    let create (degree:Degree) (switches:seq<Switch>) =
        let switchArray = switches |> Seq.toArray
        let switchCount = SwitchCount.fromInt switchArray.Length
        {
            Sorter.degree=degree;
            switchCount=switchCount;
            switches = switchArray
        }

    let fromTwoCycleArray (tc:TwoCyclePerm[]) =
        let switches = tc |> Seq.map(fun tc-> Switch.fromTwoCyclePerm tc)
                            |> Seq.concat |> Seq.toArray
        create tc.[0].degree switches
                   

    let private createWithRandomSwitches (degree:Degree) (switchCount:SwitchCount) (rnd:IRando) =
        let switches = Switch.randomSwitchesOfDegree degree rnd
                    |> Seq.take (SwitchCount.value switchCount)
                    |> Seq.toArray
        create degree switches


    let private createWithRandomStages (degree:Degree) (stageCount:StageCount)
                                       (switchFreq:SwitchFrequency) (rando:IRando) =
        let switches = (Stage.makeRandomStagedSwitchSeq degree switchFreq rando)
                        |> Seq.take ((StageCount.value stageCount) * (Degree.value degree) / 2)
                        |> Seq.toArray
        create degree switches


    let createRandom (degree:Degree) (sorterLength:SorterLength) (switchFreq:SwitchFrequency option) (rnd:IRando) =
        let swf = match switchFreq with
                        | Some f -> f
                        | None -> SwitchFrequency.fromFloat 1.0

        match sorterLength with
        | SorterLength.Switch wc -> createWithRandomSwitches degree wc rnd
        | SorterLength.Stage  tc -> createWithRandomStages degree tc swf rnd


    let createRandomArray (degree:Degree) (sorterLength:SorterLength) 
                          (switchFreq:SwitchFrequency option) (rnd:IRando) 
                          (sorterCount:SorterCount) =
        (seq {1 .. (SorterCount.value sorterCount)} 
                |> Seq.map(fun _ -> (createRandom degree sorterLength switchFreq rnd))
                |> Seq.toArray)


    let appendSwitches (switches:seq<Switch>) (sorter:Sorter) =
        let newSwitches = (switches |> Seq.toArray) |> Array.append sorter.switches
        let newSwitchCount = SwitchCount.create "" newSwitches.Length |> Result.toOption
        {
            Sorter.degree = sorter.degree;
            switchCount=newSwitchCount.Value;
            switches = (switches |> Seq.toArray) |> Array.append sorter.switches
        }

    let trimLength (sorter:Sorter) (newLength:SwitchCount) =
        if (SwitchCount.value sorter.switchCount) < (SwitchCount.value newLength) then
            "New length is longer than sorter" |> Error
        else
        let newSwitches = sorter.switches |> Array.take (SwitchCount.value newLength)
        {
            Sorter.degree = sorter.degree;
            switchCount = newLength;
            switches = newSwitches
        } |> Ok

    let mutateBySwitch (mutationRate:MutationRate) (rnd:IRando) (sorter:Sorter) =
        {
            Sorter.degree = sorter.degree;
            Sorter.switchCount = sorter.switchCount;
            switches = (Switch.mutateSwitches sorter.degree mutationRate rnd sorter.switches) |> Seq.toArray
        }

    let mutateByStage (mutationRate:MutationRate) (rnd:IRando) (sorter:Sorter) =
        let stages = Stage.mergeSwitchesIntoStages sorter.degree sorter.switches |> Seq.toArray
        let newStages = stages |> Array.map(fun st -> st |> Stage.randomMutate rnd mutationRate)
        let newSwitches = [| for stage in newStages do yield! stage.switches |]
        {
            Sorter.degree=sorter.degree;
            switchCount = (SwitchCount.create "" newSwitches.Length) |> Result.ExtractOrThrow;
            switches = newSwitches
        }

    let mutate (mutationType:SorterMutationType) (rnd:IRando) (sorter:Sorter) =
        match mutationType with
        | SorterMutationType.Switch mr -> mutateBySwitch mr rnd sorter
        | SorterMutationType.Stage mr -> mutateByStage mr rnd sorter
             

type SorterSet = {degree:Degree; sorterCount:SorterCount; sorters:Sorter[] }
module SorterSet =

    let fromSorters (degree:Degree) (sorters:seq<Sorter>) =
        let sorterArray = sorters |> Seq.toArray
        {
            degree=degree; 
            sorterCount= (SorterCount.create "" sorterArray.Length) |> Result.ExtractOrThrow; 
            sorters = sorterArray
        }

    let createRandom (degree:Degree) (sorterLength:SorterLength) (switchFreq:SwitchFrequency option)
                     (sorterCount:SorterCount) (rnd:IRando) =
        fromSorters degree 
            (seq {1 .. (SorterCount.value sorterCount)} 
                    |> Seq.map(fun _ -> (Sorter.createRandom degree sorterLength switchFreq rnd))
                    |> Seq.toArray)


//type SorterSetE = {degree:Degree; sorterCount:SorterCount;
//                    sorters:Map<EntityId, Entity<Sorter>> }
//module SorterSetE =
//    let fromSorterSet (rando1:IRando) (rando2:IRando) (sorterSet:SorterSet) =
//            {
//            degree = sorterSet.degree; 
//            sorterCount= sorterSet.sorterCount;
//            sorters = sorterSet.sorters |> Entity.createMany rando1 (Some rando2)
//                                        |> Seq.map(fun e-> (Entity.id e), e)
//                                        |> Map.ofSeq
//            }


 
type SwitchUses = private {switchCount:SwitchCount; weights:int[]}
module SwitchUses =

    let createEmpty (switchCount:SwitchCount) =
        {switchCount=switchCount; weights=Array.init (SwitchCount.value switchCount) (fun i -> 0)}

    let create (switchCount:SwitchCount) (weights:int[]) =
        if  (SwitchCount.value switchCount) = weights.Length then
            {switchCount=switchCount; weights=weights} |> Ok
        else Error (sprintf "switchCount: %d is not equal to weights length: %d" 
                             (SwitchCount.value switchCount) weights.Length) 

    let getWeights tracker = tracker.weights
    let switchCount tracker = (SwitchCount.value tracker.switchCount)

    let Add (trackerA:SwitchUses) (trackerB:SwitchUses) =
        if ((switchCount trackerA) <> (switchCount trackerB))  then
            (sprintf "switchCounts: %d, %d are not equal" 
                    (switchCount trackerA) (switchCount trackerB)) |> Error
        else
            let weightsSum = Array.map2 (+) (getWeights trackerA) (getWeights trackerB) 
            {
                switchCount = (SwitchCount.create "" weightsSum.Length) |> Result.ExtractOrThrow
                weights = weightsSum;
            } |> Ok

    let getUsedSwitches (switchUses:SwitchUses) (sorter:Sorter) =
        let useCount = SwitchCount.value switchUses.switchCount
        let switches = sorter.switches
        let weights = (getWeights switchUses)
        if (switches.Length <> useCount) then
            sprintf "useCount=%d, SwitchCount=%d" useCount switches.Length |> Error
        else
            let res = weights |> Seq.mapi(fun i w -> i,w)
                                |> Seq.filter(fun t -> (snd t) > 0 )
                                |> Seq.map(fun t -> switches.[(fst t)])
                                |> Seq.toArray
            res |> Ok


    let lastUsedIndex (st:SwitchUses) =
        let w = (getWeights st)
        w
            |> Seq.mapi(fun i x -> (i, x))
            |> Seq.filter(fun tup -> (snd tup) > 0)
            |> Seq.maxBy(fst) |> fst


    let lastUsedIndexes (switchCount:SwitchCount) (stseq:seq<SwitchUses>) =            
        let stRet = createEmpty switchCount
        let wgts = getWeights stRet
        let Recordo (stRec:int[]) (stData:SwitchUses) =
            let lui = lastUsedIndex stData
            stRec.[lui]<-stRec.[lui] + 1
        stseq |> Seq.iter(fun st -> Recordo wgts st)
        stRet

    let getSwitchUseCount (switchUses:SwitchUses) =
        SwitchCount.create "" 
            ((getWeights switchUses) |> Array.filter(fun i->i>0) |> Array.length)

    let getSwitchUseTotal (switchUses:SwitchUses) =
        (getWeights switchUses) |> Array.sum

    let entropyBits (switchUses:SwitchUses) =
        (getWeights switchUses) |> Combinatorics.entropyBits

    let getStageCount (degree:Degree) (switches:seq<Switch>) =
        StageCount.create "" 
                            ((Stage.mergeSwitchesIntoStages degree switches) |> Seq.length)

    let getRefinedStageCount (switchUses:SwitchUses) (sorter:Sorter) =
        result {
            let! usedSwitches = getUsedSwitches switchUses sorter
            let degree = sorter.degree
            return! getStageCount degree usedSwitches
        }

    let getRefinedSorter (switchUses:SwitchUses) (sorter:Sorter) =
        result {
            let! usedSwitches = getUsedSwitches switchUses sorter
            let degree = sorter.degree
            let stages = Stage.mergeSwitchesIntoStages degree usedSwitches |> Seq.toArray
            let switches = seq {for i in 0 .. (stages.Length - 1) do yield! stages.[i].switches}
            return Sorter.create degree switches
        }

    let getSwitchAndStageUses (sorter:Sorter) (switchUses:SwitchUses) =
        result
            {
                let! refinedStageCount = (getRefinedStageCount switchUses sorter)
                let! switchUseCount = (getSwitchUseCount switchUses)
                return switchUseCount, refinedStageCount
            } |> Result.ExtractOrThrow

    let reportResultStats stats =
        StringUtils.printArrayf 
            (fun res ->
            match res with
            | Ok (s,a,b,c,d) -> sprintf "%f %d %d %d" a b (SwitchCount.value c) (StageCount.value d)
            | Error msg -> sprintf "%s" msg ) 
            stats

    let reportStats stats =
        StringUtils.printArrayf 
            (fun (s,a,b,c,d) -> sprintf "%f %d %d %d" a b (SwitchCount.value c) (StageCount.value d))
            stats

    let reportEvals stats gen =
        StringUtils.printArrayf 
            (fun ((s,w,t),f) -> sprintf "%d %d %d %f" gen (SwitchCount.value w) (StageCount.value t) (SorterFitness.value f)) 
            stats