namespace Archie.Base

module Sorting =

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



    type SortableIntArray = {degree:Degree; values:int[]}
    module SortableIntArray =
        let Identity (degree:Degree) = { degree=degree; values=[|0 .. (Degree.value degree)-1|] }
        let apply f (p:SortableIntArray) = f p.values
        let value p = apply id p

        let create (degree:Degree) (vals:int[]) =
            if vals.Length <> (Degree.value degree) then
                Error (sprintf "array length %d <> degree %d:" 
                        vals.Length (Degree.value degree))
            else
                {SortableIntArray.degree=degree; values=vals } |> Ok

        let copy (sIntArray:SortableIntArray) =
            {degree=sIntArray.degree; values= apply Array.copy sIntArray}

        let fromPermutation (p:Permutation) = 
            {degree=(Permutation.degree p); values=(Permutation.arrayValues p) }

        let CreateRandoms (degree:Degree) (rnd:IRando) =
            Permutation.CreateRandoms degree rnd 
            |> Seq.map(fun i -> fromPermutation i)

        let CreateRandom (degree:Degree) (rnd:IRando) =
            Permutation.CreateRandom degree rnd 
            |> fromPermutation

        let WeightedSortableSeq (sortables:int[][]) =
            sortables |> Seq.map(fun a -> (Array.copy a, 1))

        let AllBinary (degree:Degree) =
            IntBits.AllBinaryTestCases (Degree.value degree)
            |> Seq.map(fun s -> {degree=degree; values=s})
                


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

        let MakeStagePackedSwitchSeq (rnd:IRando) (degree:Degree) =
            let aa (rnd:IRando)  = 
                (TwoCyclePerm.MakeRandomPolyCycle degree rnd )
                        |> Switch.SwitchSeqFromPolyCycle
            seq { while true do yield! (aa rnd) }


    type SorterDef = {degree:Degree; switches:array<Switch>; switchCount:SwitchCount}
     module SorterDef =

         let CreateRandom (degree:Degree) (switchCount:SwitchCount) (rnd:IRando) =
             {
                 SorterDef.degree=degree;
                 switchCount=switchCount;
                 switches = Switch.RandomSwitchesOfDegree degree rnd
                                 |> Seq.take (SwitchCount.value switchCount)
                                 |> Seq.toArray
             }

         //let CreateRandomPackedStages (degree:Degree) (switchCount:SwitchCount) (rando:IRando) =
         //    {
         //        SorterDef.degree=degree;
         //        switchCount=switchCount;
         //        switches = (Stage.MakeStagePackedSwitchSeq rando degree)
         //                        |> Seq.take (SwitchCount.value switchCount)
         //                        |> Seq.toArray
         //    }

         let AppendSwitches (switches:seq<Switch>) (sorterDef:SorterDef) =
             let newSwitches = (switches |> Seq.toArray) |> Array.append sorterDef.switches
             let newSwitchCount = SwitchCount.create "" newSwitches.Length |> Result.toOption
             {
                 SorterDef.degree = sorterDef.degree;
                 switchCount=newSwitchCount.Value;
                 switches = (switches |> Seq.toArray) |> Array.append sorterDef.switches
             }

         let TrimLength (sorterDef:SorterDef) (newLength:SwitchCount) =
             if (SwitchCount.value sorterDef.switchCount) < (SwitchCount.value newLength) then
                "New length is longer than sorter" |> Error
             else
                let newSwitches = sorterDef.switches |> Array.take (SwitchCount.value newLength)
                {
                    SorterDef.degree = sorterDef.degree;
                    switchCount = newLength;
                    switches = newSwitches
                } |> Ok
             

    type SwitchTracker = private {switchCount:SwitchCount; weights:int[]}
    module SwitchTracker =

        let create (switchCount:SwitchCount) =
            {switchCount=switchCount; weights=Array.init (SwitchCount.value switchCount) (fun i -> 0)}

        let weights tracker = tracker.weights
        let switchCount tracker = (SwitchCount.value tracker.switchCount)

        let Add (trackerA:SwitchTracker) (trackerB:SwitchTracker) =
            if ((switchCount trackerA) <> (switchCount trackerB))  then
               (sprintf "switchCounts: %d, %d are not equal" (switchCount trackerA) (switchCount trackerB))  |> Error
            else
               let weightsSum = Array.map2 (+) (weights trackerA) (weights trackerB) 
               {
                   switchCount = (SwitchCount.create "" weightsSum.Length) |> Result.ExtractOrThrow
                   weights = weightsSum;
               } |> Ok


    type SwitchUsage = {switch:Switch; switchIndex:int; useCount:int}
    module SwitchUsage =

        let CollectTheUsedSwitches (sorter:SorterDef) (tracker:SwitchTracker) = 
            seq { for i = 0 to tracker.weights.Length - 1 do
                    if (tracker.weights.[i] > 0) then
                        yield {switch=sorter.switches.[i]; switchIndex=i; 
                               useCount=tracker.weights.[i] } }
            |> Seq.toArray

        let ToString (sw:SwitchUsage) =
            sprintf "{%s, %d, %d}" (sw.switch |> Switch.ToString)
                                   sw.switchIndex sw.useCount


    type SorterGenerator = {weights:int[]}
    module SorterGenerator =
        let Make (length: int) =
            {weights=Array.init length (fun i -> 0)}