namespace Archie.Base
open System


module Combinatorics =

    let identity (degree:int) =
        [|0 .. degree-1|] 

    // Splits the sourceArray into segments using segBounds
    let breakArrayIntoSegments (sourceArray : array<'a>) (segBounds : array<int>) =
        seq {1 .. (segBounds.Length - 1) }
        |> Seq.map(fun i -> sourceArray.[segBounds.[i - 1] .. (segBounds.[i] - 1)])
        |> Seq.toArray


    let composeMapIntArrays (a:array<int>) (b:array<int>) =
        let product = Array.init a.Length (fun i -> 0)
        for i = 0 to a.Length - 1 do
           /// product.[a.[b.[i]]] <- i
           product.[i] <- a.[b.[i]]
        product
  

    let inverseMapArray (a:array<int>) =
      let aInv = Array.init a.Length (fun i -> 0)
      for i = 0 to a.Length - 1 do
          aInv.[a.[i]] <- i
      aInv

    // conj * a * conj ^ -1
    let conjugateIntArrays (a:array<int>) (conj:array<int>) =
        composeMapIntArrays conj (composeMapIntArrays a (inverseMapArray conj) )

    // returns a sequence of draws from initialList without replacement. 
    // Does not change initialList
    let fisherYatesShuffle (rnd:IRando) (initialList:array<'a>) =
        let rndmx max = rnd.NextUInt % max
        let availableFlags = Array.init initialList.Length (fun i -> (i, true))
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


    let randomPermutation (rnd:IRando) (degree:int) =
         (fisherYatesShuffle rnd)  [|0 .. degree-1|] |> Seq.toArray


    let randomPermutations (rnd:IRando) (degree:int) =
         Seq.initInfinite (fun n -> randomPermutation rnd degree)


    let isSorted (values:int[]) =
        let mutable i=1
        let mutable looP = true
        while ((i < values.Length) && looP) do
             looP <- (values.[i-1] <= values.[i])
             i<-i+1
        looP

    let spanIsSorted (values:Span<int>) =
        let mutable i=1
        let mutable looP = true
        while ((i < values.Length) && looP) do
             looP <- (values.[i-1] <= values.[i])
             i<-i+1
        looP


    let isSortedOffset (baseValues:int[]) (offset:int) (length:int) =
        let mutable i=1
        let mutable looP = true
        while ((i < length) && looP) do
             looP <- (baseValues.[i+offset-1] <= baseValues.[i+offset])
             i<-i+1
        looP

    let isTwoCycle (a:int[]) =
        (composeMapIntArrays a a) = identity a.Length

    let distanceSquared (a:array<int>) (b:array<int>) =
        Array.fold2 (fun acc elem1 elem2 ->
        acc + (elem1 - elem2) * (elem1 - elem2)) 0 a b

    // Measured in bits (log base 2)
    let entropyBits (a:array<int>) =
        let f = 1.0 / Math.Log(2.0)
        let tot = float (a |> Array.sum)
        let fa = a  |> Array.filter(fun i->i>0)
                    |> Array.map (fun i->(float i) / tot)
        let res = Array.fold (fun acc elem -> acc - elem * f * Math.Log(elem)) 0.0 fa
        res 


    let unsortednessSquared (a:array<int>) =
        distanceSquared a [|0 .. (a.Length - 1)|]


    // will do conventional sort if the stage array is all 1 or 2 cycles
    let sortIntArray (sortable: array<int>) (stage:array<int>) (counter:array<int>) =
        for i = 0 to stage.Length - 1 do
            let j = stage.[i]
            if (j > i ) then
                let stbA = sortable.[i]
                let stbB = sortable.[j]
                if(stbB < stbA) then
                    sortable.[i] <- stbB
                    sortable.[j] <- stbA
                    counter.[i] <- counter.[i] + 1


    let sortCopyOfIntArray (sortable: array<int>) (stage:array<int>) (counter:array<int>) =
        let stbC = Array.copy sortable
        sortIntArray stbC stage counter
        stbC


    let makeMonoTwoCycle (degree:Degree) (aDex:int) (bDex:int) =
        Array.init (Degree.value degree) (fun i -> 
            if   (i = aDex) then bDex
            elif (i = bDex) then aDex
            else i)


    let drawTwoWithoutRep (degree:Degree) (rnd:IRando) =
        let aBit = rnd.NextPositiveInt % Degree.value(degree)
        let mutable bBit = rnd.NextPositiveInt % Degree.value(degree)
        while aBit = bBit do
            bBit <- rnd.NextPositiveInt % Degree.value(degree)
        if aBit < bBit then aBit, bBit
        else bBit, aBit


    let makeRandomMonoTwoCycle (degree:Degree) (rnd:IRando) =
        let tup = drawTwoWithoutRep degree rnd
        makeMonoTwoCycle degree (fst tup) (snd tup)


    let makeAllMonoTwoCycles (degree:Degree) =
        seq {for i = 0 to (Degree.value(degree) - 1) do
                for j = 0 to i - 1 do
                    yield makeMonoTwoCycle degree i j}


    let makeRandomTwoCycleIntArray (rnd:IRando) (arraysize:int) (cycleCount:int) =
        let initialList = [|0 .. arraysize-1|]
        let arrayRet = Array.init arraysize (fun i -> i)
        let rndTupes = (fisherYatesShuffle rnd initialList) |> (Seq.chunkBySize 2) |> Seq.toArray
        for i = 0 to cycleCount - 1 do
            arrayRet.[rndTupes.[i].[0]] <- rndTupes.[i].[1]
            arrayRet.[rndTupes.[i].[1]] <- rndTupes.[i].[0]
        arrayRet


    let makeRandomFullTwoCycleIntArray (rnd:IRando) (arraysize:int) =
        makeRandomTwoCycleIntArray rnd arraysize (arraysize/2)


    let MakeRandomFullTwoCycleIntArrays (rnd:IRando) (arraysize:int) (count:int) =
        seq {1 .. count} |> Seq.map (fun i -> makeRandomFullTwoCycleIntArray rnd arraysize)


    // bins is an increasing set of positive numbers.
    // returns the index of the first member e>value.
    let findBin (bins:float[]) (value:float) =
        bins |> Array.findIndex(fun b -> b>value)


    let makeBins (weights:float[]) =
        let mutable tot = 0.0
        let cumo w =
            tot<-tot + w
            tot
        weights |> Array.map(fun w -> cumo w)
        

    let drawFromWeightedDistribution (fitnessFunc:'a->SorterFitness) (rnd:IRando) (items:'a[]) =
        let bins = items |> Array.map(fun it-> (SorterFitness.value (fitnessFunc it)))
                         |> makeBins
        let maxVal = bins.[bins.Length - 1]
        Seq.initInfinite(fun _-> items.[findBin bins (rnd.NextFloat * maxVal)])


    let makeHull (seqX:seq<'a>) (seqY:seq<'a>) (x:int) (y:int) =
        let xa = seqX |> Seq.take(x) |> Seq.toArray
        let ya = seqY |> Seq.take(y) |> Seq.toArray
        seq {for i = 0 to x - 1 do yield (xa.[i], ya.[y-1])}
            |> Seq.append (seq {for i = 0 to y - 2 do yield (xa.[x-1], ya.[i])})
