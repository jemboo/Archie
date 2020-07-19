namespace Archie.Base
open Archie.Base.Combinatorics_Types

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
            SwitchSeqFromIntArray (Permutation.value p)
     
        let SwitchSeqFromPolyCycle (p:TwoCycleIntArray) =
            SwitchSeqFromIntArray (TwoCycleIntArray.value p)
    
        let ToString (sw:Switch) =
            sprintf "(%d, %d)" sw.low sw.hi

        let RandomSwitchesOfDegree (order:Degree) (rnd:IRando) =
            let mDex = uint32 ((Degree.value order)*(Degree.value order + 1) / 2) 
            seq { while true do 
                     let p = (int (rnd.NextUInt % mDex))
                     yield SwitchMap.[p] }



    type SortableIntArray = {values:int[]}
    module SortableIntArray =

        let Identity (order: int) = { SortableIntArray.values = [|0 .. order-1|] }
        let apply f (p:SortableIntArray) = f p.values
        let value p = apply id p

        let SwitchFuncForSwitch (sw:Switch) =
            fun (x:int[]) -> 
                if (x.[sw.low] > x.[sw.hi]) then
                    let lv = x.[sw.low]
                    x.[sw.low] <- x.[sw.hi]
                    x.[sw.hi] <- lv
                    (x, true)
                else (x, false)

        let CreateRandom (order:int) (rnd : IRando) =
            Permutation.CreateRandom rnd order
            |> Seq.map(fun i -> { SortableIntArray.values = Permutation.value i })

        let SortableSeq (sortables:int[][]) =
            sortables
                |> Array.map(fun a -> Array.copy a)
                |> Array.toSeq

        let WeightedSortableSeq (sortables:int[][]) =
            sortables
                |> Seq.map(fun a -> (Array.copy a, 1))

        let SortableSeqAllBinary (degree:Degree) =
                IntBits.AllBinaryTestCases (Degree.value degree)


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
                (TwoCycleIntArray.MakeRandomPolyCycle rnd (Degree.value degree))
                        |> Switch.SwitchSeqFromPolyCycle
            seq { while true do yield! (aa rnd) }


    type SorterDef = {degree:Degree; switches: array<Switch>; switchCount:SwitchCount}
     module SorterDef =

         //let Equals (sdA:SorterDef) (sdB:SorterDef) =
         //   if sdA.order <> sdB.order then false
         //   elif sdA.switches.Length <> sdB.switches.Length then false
         //   else seq { for r in 0 .. sdA.switches.Length - 1 do
         //              if sdA.switches.[r] <> sdB.switches.[r]  then
         //                yield false }
         //           |> Seq.forall id

         let CreateRandom (degree:Degree) (switchCount:SwitchCount) (rnd:IRando) =
             {
                 SorterDef.degree=degree;
                 switchCount=switchCount;
                 switches = Switch.RandomSwitchesOfDegree degree rnd
                                 |> Seq.take (SwitchCount.value switchCount)
                                 |> Seq.toArray
             }

         let CreateRandomPackedStages (degree:Degree) (switchCount:SwitchCount) (rando:IRando) =
             {
                 SorterDef.degree=degree;
                 switchCount=switchCount;
                 switches = (Stage.MakeStagePackedSwitchSeq rando degree)
                                 |> Seq.take (SwitchCount.value switchCount)
                                 |> Seq.toArray
             }

         let AppendSwitches (switches:seq<Switch>) (sorterDef:SorterDef) =
             let newSwitches = (switches |> Seq.toArray) |> Array.append sorterDef.switches
             let newSwitchCount = SwitchCount.create "" newSwitches.Length |> Result.toOption
             {
                 SorterDef.degree = sorterDef.degree;
                 switchCount=newSwitchCount.Value;
                 switches = (switches |> Seq.toArray) |> Array.append sorterDef.switches
             }

    type SwitchTracker = {weights:int[]}
    module SwitchTracker =
        let Make (switchCount: SwitchCount) =
            {weights=Array.init (SwitchCount.value switchCount) (fun i -> 0)}



        //let ToStageArrays (switchTracker:SwitchTracker) 
        //                  (sorterDef:SorterDef) =

        //    Combinatorics.BreakArrayIntoSegments 
        //        switchTracker.weights 
        //        stagedSorterDef.stageIndexes


        //let ToStageReportString 
        //                (switchTracker:SwitchTracker) 
        //                (stagedSorterDef:StagedSorterDef) =

        //    let ArrayToString(array:'a[]) =
        //        sprintf "(%s)" (array |> Seq.map(fun a -> string a) |> String.concat ", ")

        //    let NestedArrayToString (af: 'a[]->string) (nestedArray:'a[][]) =
        //        sprintf "(%s)" (nestedArray |> Seq.map(fun a -> ArrayToString a) |> String.concat ", ")

        //    NestedArrayToString ArrayToString (ToStageArrays switchTracker stagedSorterDef)



    type SwitchUsage = {switch:Switch; switchIndex:int; useCount:int}
    module SwitchUsage =

        let CollectTheUsedSwitches 
                    (sorterDef:SorterDef) 
                    (switchTracker:SwitchTracker) = 
            seq { for i = 0 to switchTracker.weights.Length - 1 do
                    if (switchTracker.weights.[i] > 0) then
                        yield {switch=sorterDef.switches.[i]; switchIndex=i; 
                               useCount=switchTracker.weights.[i] } }
            |> Seq.toArray

        let ToString (sw:SwitchUsage) =
            sprintf "{%s, %d, %d}" (sw.switch |> Switch.ToString)
                                   sw.switchIndex sw.useCount


    type SorterGenerator = {weights:int[]}
    module SorterGenerator =
        let Make (length: int) =
            {weights=Array.init length (fun i -> 0)}