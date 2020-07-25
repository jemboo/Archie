﻿namespace Archie.Base
open System

module Combinatorics =

    let Random_0_1 (rnd : IRando) (len: int) (pctOnes:float) =
        Seq.init len (fun n -> if (rnd.NextFloat > pctOnes) then 0 else 1)

    // Splits the sourceArray into segments using segBounds
    let BreakArrayIntoSegments (sourceArray : array<'a>) 
                               (segBounds : array<int>) =
        seq {1 .. (segBounds.Length - 1) }
        |> Seq.map(fun i -> sourceArray.[segBounds.[i - 1] .. (segBounds.[i] - 1)])
        |> Seq.toArray

    let FisherYatesShuffle (rnd:IRando) (initialList : array<'a>) =
        let rndmx max = rnd.NextUInt % max
        let availableFlags = Array.init initialList.Length (fun i -> (i, true))
                                                          // Which items are available and their indices
        let nextItem nLeft =
            let nItem = (rndmx nLeft)                     // Index out of available items
            let index =                                   // Index in original deck
                availableFlags                            // Go through available array
                |> Seq.filter (fun (ndx,f) -> f)          // and pick out only the available tuples
                |> Seq.item (int nItem)                   // Get the one at our chosen index
                |> fst                                    // and retrieve it's index into the original array
            availableFlags.[index] <- (index, false)      // Mark that index as unavailable
            initialList.[index]                           // and return the original item
        seq {(initialList.Length) .. -1 .. 1}             // Going from the length of the list down to 1
        |> Seq.map (fun i -> nextItem (uint32 i))         // yield the next item


    let IsSorted (values:int[]) =
        seq{for i=1 to values.Length - 1 do
                if values.[i-1] > values.[i] then
                    yield false} |> Seq.forall id


    let Int_To_IntArray01 (len:int) (intVers:int) =
        let bitLoc (loc:int) (intBits:int) =
            if (((1 <<< loc) &&& intBits) <> 0) then 1 else 0
        Array.init len (fun i -> bitLoc i intVers)

    let IntArray01_To_Int (len:int) (arrayVers:int[]) =
        let mutable intRet = 0
        let bump i =
            if (arrayVers.[i] = 1) then
                intRet <- intRet + 1
            intRet <- intRet * 2

        {1 .. len} |> Seq.iter(fun i -> bump i)
        intRet

    let RandomIntPermutations (rnd :IRando) (len: int) (count:int) =
         FisherYatesShuffle rnd [|0 .. len-1|] |> Seq.toArray


    let CompareArrays (a: array<int>) (b: array<int>) =
        if (a.Length <> b.Length) then false
        else seq {for i = 0 to a.Length - 1 do
                    if (a.[i] <> b.[i]) then yield false}
                 |> Seq.forall id
  
    let InverseMapArray (a:array<int>) =
        let aInv = Array.init a.Length (fun i -> 0)
        for i = 0 to a.Length - 1 do
            aInv.[a.[i]] <- i
        aInv
      
    let ComposeMapIntArrays (a:array<int>) (b:array<int>) =
        let product = Array.init a.Length (fun i -> 0)
        for i = 0 to a.Length - 1 do
            product.[a.[b.[i]]] <- i
        product
  
    // conj * a * conj ^ -1
    let ConjugateIntArrays (a:array<int>) (conj:array<int>) =
        conj |> ComposeMapIntArrays a |> ComposeMapIntArrays (InverseMapArray conj)

    let DistanceSquared (a:array<int>) (b:array<int>) =
        Array.fold2 (fun acc elem1 elem2 ->
        acc + (elem1 - elem2) * (elem1 - elem2)) 0 a b

    let UnsortednessSquared (a:array<int>) =
        DistanceSquared a [|0 .. (a.Length - 1)|]

    // will do conventional sort if the stage array is all 1 or 2 cycles
    let SortIntArray (sortable: array<int>) (stage:array<int>) (counter:array<int>) =
        for i = 0 to stage.Length - 1 do
            let j = stage.[i]
            if (j > i ) then
                let stbA = sortable.[i]
                let stbB = sortable.[j]
                if(stbB < stbA) then
                    sortable.[i] <- stbB
                    sortable.[j] <- stbA
                    counter.[i] <- counter.[i] + 1

    let SortCopyOfIntArray (sortable: array<int>) (stage:array<int>) (counter:array<int>) =
        let stbC = Array.copy sortable
        SortIntArray stbC stage counter
        stbC

    let MakeTwoCycleIntArray (arraysize: int) (lowBit:int) (hiBit:int) =
        Array.init arraysize (fun i -> if   (i = lowBit) then hiBit
                                       elif (i = hiBit) then lowBit
                                       else i)

    let MakeAllTwoCycleIntArrays (arraysize: int) =
        seq {for i = 0 to arraysize - 1 do
                for j = 0 to i - 1 do
                    yield MakeTwoCycleIntArray arraysize i j}

    let MakeRandomFullTwoCycleIntArray (rnd : IRando) (arraysize:int) =
        let initialList = [|0 .. arraysize-1|]
        let arrayRet = Array.init arraysize (fun i -> i)
        let rndTupes = (FisherYatesShuffle rnd initialList) |> (Seq.chunkBySize 2) |> Seq.toArray
        for i = 0 to (arraysize / 2) - 1 do
            arrayRet.[rndTupes.[i].[0]] <- rndTupes.[i].[1]
            arrayRet.[rndTupes.[i].[1]] <- rndTupes.[i].[0]
        arrayRet

    let MakeRandomFullTwoCycleIntArray2 (rnd : IRando) (arraysize:int) =
        let initialList = [|0 .. arraysize-1|]
        let arrayRet = Array.init arraysize (fun i -> i)
        let rndTupes = (FisherYatesShuffle rnd initialList) |> (Seq.chunkBySize 2) |> Seq.toArray
        for i = 0 to (arraysize / 2) - 1 do
            arrayRet.[rndTupes.[i].[0]] <- rndTupes.[i].[1]
            arrayRet.[rndTupes.[i].[1]] <- rndTupes.[i].[0]
        arrayRet

    let MakeRandomFullTwoCycleIntArrays (rnd : IRando) (arraysize:int) (count:int) =
        seq {1 .. count} |> Seq.map (fun i -> MakeRandomFullTwoCycleIntArray rnd arraysize)


module Combinatorics_Types =

    type Permutation = private {degree:Degree; values:int[] }
    module Permutation =
        let create (degree:Degree) (vals:int[]) =
            if vals.Length <> (Degree.value degree) then
                Error (sprintf "array length %d <> degree %d:" 
                        vals.Length (Degree.value degree))
            else
                {Permutation.degree=degree; values=vals } |> Ok

        let Identity (degree:Degree) = 
            {degree=degree; values=[|0 .. (Degree.value degree)-1|] }

        let value perm = perm.values
        let degree perm = perm.degree

        let CreateRandom (degree:Degree) (rnd:IRando) =
            let initialList = (Identity degree) |> value                             
            seq { while true do 
                    yield { degree=degree;
                            values=(Combinatorics.FisherYatesShuffle rnd initialList |> Seq.toArray)}}

        let Inverse (p:Permutation) =
            create p.degree (Combinatorics.InverseMapArray (p |> value))
 
        let Product (pA:Permutation) (pB:Permutation) =
            if (Degree.value pA.degree) <> (Degree.value pB.degree) then
                    Error (sprintf "degree %d <> degree %d:" 
                            (Degree.value pA.degree) (Degree.value pB.degree))
            else
                create pA.degree  (Combinatorics.ComposeMapIntArrays (pA |> value) (pB |> value))
 

    type TwoCycleIntArray = private TwoCycleIntArray of int[]
    module TwoCycleIntArray =

        let Identity (order: int) = TwoCycleIntArray [|0 .. order-1|]
        let apply f (TwoCycleIntArray p) = f p
        let value p = apply id p

        let MakeMonoCycleFromHiLow (order:int) (hi:int) (low:int) =
            TwoCycleIntArray (Combinatorics.MakeTwoCycleIntArray order low hi)

        let MakeAllMonoCycleOfLength (order:int) =
            (Combinatorics.MakeAllTwoCycleIntArrays order) 
            |> Seq.map (fun s -> TwoCycleIntArray s)
         
        let MakeRandomPolyCycle (rnd:IRando) (order: int) =
            TwoCycleIntArray (Combinatorics.MakeRandomFullTwoCycleIntArray rnd order)

        let MakeRandomPolyCycle2 (rnd:IRando) (order: int) =
            TwoCycleIntArray (Combinatorics.MakeRandomFullTwoCycleIntArray2 rnd order)


    type BitArray = {order:int; items: array<bool>}
    module BitArray =
        let Zero (order: int) =  { order=order; items=Array.init order (fun i -> false) }
        let Next (bits: BitArray) =  { order=bits.order; items=bits.items }


    module IntBits =
        let Sorted_O_1_Sequence (blockLen:int) (onesCount:int) =
            seq {for i = 1 to blockLen - onesCount do yield 0; 
                 for i = 1 to onesCount do yield 1 }

        //Returns a bloclLen + 1 length array of all possible sorted 0-1 sequences of length blockLen
        let Sorted_0_1_Sequences (blockLen:int) =
            seq {for i = 0 to blockLen 
                    do yield (Sorted_O_1_Sequence blockLen i) |> Seq.toArray }
                |> Seq.toArray

        let AllBinaryTestCases (order:int) =
            {0 .. (1 <<< order) - 1}
            |> Seq.map (fun i -> Combinatorics.Int_To_IntArray01 order i)
        