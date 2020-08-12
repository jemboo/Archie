namespace Archie.Base
open System
open System.Collections.Generic

module SorterParts =

    [<Struct>]
    type Switch = { low: int; hi: int }
    module Switch =

        let switchMap = 
            [for hi=0 to 64 
                do for low=0 to hi do yield {Switch.low=low; Switch.hi=hi}]

        let zeroSwitches =
            seq { while true do yield {Switch.low=0; Switch.hi=0}}

        let switchSeqFromIntArray (pArray:int[]) =
             seq { for i = 0 to pArray.Length - 1 do
                    let j = pArray.[i]
                    if ((j > i ) && (i = pArray.[j]) ) then
                         yield {Switch.low=i; Switch.hi=j} }

        let switchSeqFromPermutation (p:Permutation) =
            switchSeqFromIntArray (Permutation.arrayValues p)
     
        let switchSeqFromPolyCycle (p:TwoCyclePerm) =
            switchSeqFromIntArray (TwoCyclePerm.arrayValues p)
    
        let toString (sw:Switch) =
            sprintf "(%d, %d)" sw.low sw.hi

        let randomSwitchesOfDegree (order:Degree) (rnd:IRando) =
            let mDex = uint32 ((Degree.value order)*(Degree.value order + 1) / 2) 
            seq { while true do 
                     let p = (int (rnd.NextUInt % mDex))
                     yield switchMap.[p] }


    type SortableSet = {degree:Degree; baseArray:int[]}
    module SortableSet =
        let create (degree:Degree) (baseArray:int[]) =
            if baseArray.Length < 0 + (Degree.value degree) then
                Error (sprintf "baseArray length %d is not a multiple of degree: %d:" 
                        baseArray.Length (Degree.value degree))
            else
                let baseCopy = Array.zeroCreate baseArray.Length
                Array.Copy(baseArray, baseCopy, baseArray.Length)
                {degree=degree; baseArray=baseCopy} |> Ok

        let copy (sortableSet:SortableSet) =
            let baseCopy = Array.zeroCreate sortableSet.baseArray.Length
            Array.Copy(sortableSet.baseArray, baseCopy, baseCopy.Length)
            {degree=sortableSet.degree; baseArray=baseCopy;}

        let copy2 (sortableSet:SortableSet) =
            let baseCopy = Array.create sortableSet.baseArray.Length 0
            Array.Copy(sortableSet.baseArray, baseCopy, baseCopy.Length)
            {degree=sortableSet.degree; baseArray=baseCopy;}

        let allBinary (degree:Degree) =
            let baseArray = IntBits.AllBinaryTestCasesArray (Degree.value degree)
                            |> Array.collect(fun a -> a)
            create degree baseArray


    type Stage = {switches:Switch list; degree:Degree}
    module Stage =
        let mergeSwitchesIntoStages (degree:Degree) (switches:seq<Switch>) =
            let mutable stageTracker = Array.init (Degree.value degree) (fun i -> false)
            let switchesForStage = new ResizeArray<Switch>()
            seq { 
                    for sw in switches do
                        if (stageTracker.[sw.hi] || stageTracker.[sw.low] ) then
                            yield { Stage.switches = switchesForStage |> Seq.toList; degree = degree}
                            stageTracker <- Array.init (Degree.value degree) (fun i -> false)
                            switchesForStage.Clear()
                        stageTracker.[sw.hi] <- true
                        stageTracker.[sw.low] <- true
                        switchesForStage.Add sw
                    yield { Stage.switches=switchesForStage |> Seq.toList;  degree = degree}
                 }

        let getStageIndexesFromSwitches (order:int) (switches:seq<Switch>) =
            let mutable stageTracker = Array.init order (fun i -> false)
            let mutable curDex = 0
            seq { 
                    yield curDex
                    for sw in switches do
                        if (stageTracker.[sw.hi] || stageTracker.[sw.low] ) then
                            yield curDex
                            stageTracker <- Array.init order (fun i -> false)
                        stageTracker.[sw.hi] <- true
                        stageTracker.[sw.low] <- true
                        curDex <- curDex + 1
                    yield curDex
                 }

        let makeStagePackedSwitchSeq (degree:Degree) (rnd:IRando)=
            let aa (rnd:IRando)  = 
                (TwoCyclePerm.MakeRandomFullTwoCycle degree rnd )
                        |> Switch.switchSeqFromPolyCycle
            seq { while true do yield! (aa rnd) }


    type Sorter = {degree:Degree; switches:array<Switch>; switchCount:SwitchCount}
     module Sorter =
        
         let create (degree:Degree) (switches:seq<Switch>) =
             let switchArray = switches |> Seq.toArray
             let switchCount = SwitchCount.create "" switchArray.Length |> Result.ExtractOrThrow
             {
                Sorter.degree=degree;
                switchCount=switchCount;
                switches = switchArray
             }

         let createRandom (degree:Degree) (switchCount:SwitchCount) (rnd:IRando) =
             {
                 Sorter.degree=degree;
                 switchCount=switchCount;
                 switches = Switch.randomSwitchesOfDegree degree rnd
                                 |> Seq.take (SwitchCount.value switchCount)
                                 |> Seq.toArray
             }

         let createRandomPackedStages (degree:Degree) (stageCount:StageCount) (rando:IRando) =
             let switchCount = StageCount.ToSwitchCount degree stageCount |> Result.ExtractOrThrow
             {
                 Sorter.degree=degree;
                 switchCount = switchCount
                 switches = (Stage.makeStagePackedSwitchSeq degree rando )
                                 |> Seq.take (SwitchCount.value switchCount)
                                 |> Seq.toArray
             }

         let appendSwitches (switches:seq<Switch>) (sorterDef:Sorter) =
             let newSwitches = (switches |> Seq.toArray) |> Array.append sorterDef.switches
             let newSwitchCount = SwitchCount.create "" newSwitches.Length |> Result.toOption
             {
                 Sorter.degree = sorterDef.degree;
                 switchCount=newSwitchCount.Value;
                 switches = (switches |> Seq.toArray) |> Array.append sorterDef.switches
             }

         let trimLength (sorterDef:Sorter) (newLength:SwitchCount) =
             if (SwitchCount.value sorterDef.switchCount) < (SwitchCount.value newLength) then
                "New length is longer than sorter" |> Error
             else
                let newSwitches = sorterDef.switches |> Array.take (SwitchCount.value newLength)
                {
                    Sorter.degree = sorterDef.degree;
                    switchCount = newLength;
                    switches = newSwitches
                } |> Ok
             

    type SorterSet = {degree:Degree; sorterCount:SorterCount; sorters:Sorter[] }
    module SorterSet =
        let fromSorters (degree:Degree) (sorters:seq<Sorter>) =
            let sorterArray = sorters |> Seq.toArray
            {
               degree=degree; 
               sorterCount= (SorterCount.create "" sorterArray.Length) |> Result.ExtractOrThrow; 
               sorters = sorterArray
            }

        let createRandomStagePacked (degree:Degree) (stageCount:StageCount) 
                                    (sorterCount:SorterCount) (rando:IRando) =
            fromSorters degree (seq {1 .. (SorterCount.value sorterCount)} 
                                    |> Seq.map(fun _ -> (Sorter.createRandomPackedStages degree stageCount rando))
                                    |> Seq.toArray)


        let createRandom (degree:Degree) (switchCount:SwitchCount) 
                         (sorterCount:SorterCount) (rando:IRando) =
            fromSorters degree (seq {1 .. (SorterCount.value sorterCount)} 
                                    |> Seq.map(fun _ -> (Sorter.createRandom degree switchCount rando))
                                    |> Seq.toArray)


    type SorterSetE = {degree:Degree; sorterCount:SorterCount; 
                       sorters:Map<EntityId, Entity<Sorter>> }
    module SorterSetE =
        let fromSorterSet (rando1:IRando) (rando2:IRando) (sorterSet:SorterSet) =
             {
                degree = sorterSet.degree; 
                sorterCount= sorterSet.sorterCount;
                sorters = sorterSet.sorters |> Entity.createMany rando1 rando2
                                            |> Seq.map(fun e-> (Entity.id e), e)
                                            |> Map.ofSeq
             }


 
    type SwitchUses = private {switchCount:SwitchCount; weights:int[]}
    module SwitchUses =

        let create (switchCount:SwitchCount) =
            {switchCount=switchCount; weights=Array.init (SwitchCount.value switchCount) (fun i -> 0)}

        let getWeights tracker = tracker.weights
        let switchCount tracker = (SwitchCount.value tracker.switchCount)

        //let Add (trackerA:SwitchUses) (trackerB:SwitchUses) =
        //    if ((switchCount trackerA) <> (switchCount trackerB))  then
        //       (sprintf "switchCounts: %d, %d are not equal" 
        //                (switchCount trackerA) (switchCount trackerB)) |> Error
        //    else
        //       let weightsSum = Array.map2 (+) (getWeights trackerA) (getWeights trackerB) 
        //       {
        //           switchCount = (SwitchCount.create "" weightsSum.Length) |> Result.ExtractOrThrow
        //           weights = weightsSum;
        //       } |> Ok

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
            let stRet = create switchCount
            let wgts = getWeights stRet
            let Recordo (stRec:int[]) (stData:SwitchUses) =
                let lui = lastUsedIndex stData
                stRec.[lui]<-stRec.[lui] + 1
            stseq |> Seq.iter(fun st -> Recordo wgts st)
            stRet

        let useCount (switchUses:SwitchUses) =
            (getWeights switchUses) |> Array.filter(fun i->i>0) |> Array.length

        let useTotal (switchUses:SwitchUses) =
            (getWeights switchUses) |> Array.sum

        let entropyBits (switchUses:SwitchUses) =
            (getWeights switchUses) |> Combinatorics.EntropyBits

        let getStageCount (sorter:Sorter) =
            (Stage.mergeSwitchesIntoStages sorter.degree sorter.switches)
                    |> Seq.length

        let getRefinedStageCount (switchUses:SwitchUses) (sorter:Sorter) =
            result {
                let! usedSwitches = getUsedSwitches switchUses sorter
                let degree = sorter.degree
                return Stage.mergeSwitchesIntoStages degree usedSwitches |> Seq.length
            }

        let getRefinedSorter (switchUses:SwitchUses) (sorter:Sorter) =
            result {
                let! usedSwitches = getUsedSwitches switchUses sorter
                let degree = sorter.degree
                let stages = Stage.mergeSwitchesIntoStages degree usedSwitches |> Seq.toArray
                let switches = seq {for i in 0 .. (stages.Length - 1) do yield! stages.[i].switches}
                return Sorter.create degree switches
            }

        let getStats (tup) =
            tup |> fun tup -> entropyBits (fst tup), 
                                               useCount (fst tup), 
                                               useTotal (fst tup)


        //let getStats (switchUses:SwitchUses) (sorter:Sorter option) =
        //    (switchUses, sorter) |> fun tup -> entropyBits (fst tup), 
        //                                       useCount (fst tup), 
        //                                       useTotal (fst tup)

