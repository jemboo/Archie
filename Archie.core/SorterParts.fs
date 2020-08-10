namespace Archie.Base
open System

module SorterParts =

    [<Struct>]
    type Switch = { low: int; hi: int }
    module Switch =

        let SwitchMap = 
            [for hi=0 to 64 
                do for low=0 to hi do yield {Switch.low=low; Switch.hi=hi}]

        let ZeroSwitches =
            seq { while true do yield {Switch.low=0; Switch.hi=0}}

        let SwitchSeqFromIntArray (pArray:int[]) =
             seq { for i = 0 to pArray.Length - 1 do
                    let j = pArray.[i]
                    if ((j > i ) && (i = pArray.[j]) ) then
                         yield {Switch.low=i; Switch.hi=j} }

        let SwitchSeqFromPermutation (p:Permutation) =
            SwitchSeqFromIntArray (Permutation.arrayValues p)
     
        let SwitchSeqFromPolyCycle (p:TwoCyclePerm) =
            SwitchSeqFromIntArray (TwoCyclePerm.arrayValues p)
    
        let ToString (sw:Switch) =
            sprintf "(%d, %d)" sw.low sw.hi

        let RandomSwitchesOfDegree (order:Degree) (rnd:IRando) =
            let mDex = uint32 ((Degree.value order)*(Degree.value order + 1) / 2) 
            seq { while true do 
                     let p = (int (rnd.NextUInt % mDex))
                     yield SwitchMap.[p] }

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
        let MergeSwitchesIntoStages (degree:Degree) (switches:seq<Switch>) =
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

        let GetStageIndexesFromSwitches (order:int) (switches:seq<Switch>) =
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

        let MakeStagePackedSwitchSeq (degree:Degree) (rnd:IRando)=
            let aa (rnd:IRando)  = 
                (TwoCyclePerm.MakeRandomFullTwoCycle degree rnd )
                        |> Switch.SwitchSeqFromPolyCycle
            seq { while true do yield! (aa rnd) }


    type Sorter = {degree:Degree; switches:array<Switch>; switchCount:SwitchCount}
     module Sorter =

         let CreateRandom (degree:Degree) (switchCount:SwitchCount) (rnd:IRando) =
             {
                 Sorter.degree=degree;
                 switchCount=switchCount;
                 switches = Switch.RandomSwitchesOfDegree degree rnd
                                 |> Seq.take (SwitchCount.value switchCount)
                                 |> Seq.toArray
             }

         let CreateRandomPackedStages (degree:Degree) (stageCount:StageCount) (rando:IRando) =
             let switchCount = StageCount.ToSwitchCount degree stageCount |> Result.ExtractOrThrow
             {
                 Sorter.degree=degree;
                 switchCount = switchCount
                 switches = (Stage.MakeStagePackedSwitchSeq degree rando )
                                 |> Seq.take (SwitchCount.value switchCount)
                                 |> Seq.toArray
             }

         let AppendSwitches (switches:seq<Switch>) (sorterDef:Sorter) =
             let newSwitches = (switches |> Seq.toArray) |> Array.append sorterDef.switches
             let newSwitchCount = SwitchCount.create "" newSwitches.Length |> Result.toOption
             {
                 Sorter.degree = sorterDef.degree;
                 switchCount=newSwitchCount.Value;
                 switches = (switches |> Seq.toArray) |> Array.append sorterDef.switches
             }

         let TrimLength (sorterDef:Sorter) (newLength:SwitchCount) =
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

        //let fromSorterMaker (degree:Degree) (sorterCount:SorterCount) (maker:Func)

        let createRandomStagePacked (degree:Degree) (stageCount:StageCount) 
                                    (sorterCount:SorterCount) (rando:IRando) =
            fromSorters degree (seq {1 .. (SorterCount.value sorterCount)} 
                                    |> Seq.map(fun _ -> (Sorter.CreateRandomPackedStages degree stageCount rando))
                                    |> Seq.toArray)


        let createRandom (degree:Degree) (switchCount:SwitchCount) 
                         (sorterCount:SorterCount) (rando:IRando) =
            fromSorters degree (seq {1 .. (SorterCount.value sorterCount)} 
                                    |> Seq.map(fun _ -> (Sorter.CreateRandom degree switchCount rando))
                                    |> Seq.toArray)


    type SorterSetE = {degree:Degree; sorterCount:SorterCount; sorters:Entity<Sorter>[] }
    module SorterSetE =
        let fromSorterSet (sorterSet:SorterSet) (rando1:IRando) (rando2:IRando) =
             {
                degree = sorterSet.degree; 
                sorterCount= sorterSet.sorterCount;
                sorters = sorterSet.sorters |> (Entity.createMany rando1 rando2) |> Seq.toArray
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

        let LastUsedIndex (st:SwitchUses) =
            let w = (getWeights st)
            w
               |> Seq.mapi(fun i x -> (i, x))
               |> Seq.filter(fun tup -> (snd tup) > 0)
               |> Seq.maxBy(fst) |> fst

        let LastUsedIndexes (switchCount:SwitchCount) (stseq:seq<SwitchUses>) =            
            let stRet = create switchCount
            let wgts = getWeights stRet
            let Recordo (stRec:int[]) (stData:SwitchUses) =
                let lui = LastUsedIndex stData
                stRec.[lui]<-stRec.[lui] + 1
            stseq |> Seq.iter(fun st -> Recordo wgts st)
            stRet

        let UseCount (st:SwitchUses) =
            (getWeights st) |> Array.filter(fun i->i>0) |> Array.length

        let UseTotal (st:SwitchUses) =
            (getWeights st) |> Array.sum

        let EntropyBits (sorterSet:SwitchUses) =
            (getWeights sorterSet) |> Combinatorics.EntropyBits

