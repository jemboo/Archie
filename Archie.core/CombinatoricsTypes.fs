namespace Archie.Base

// a permutation of the set {0, 1,.. (degree-1)}
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

    let arrayValues perm = perm.values
    let degree perm = perm.degree

    let CreateRandom (degree:Degree) (rnd:IRando) =
        let idArray = (Identity degree) |> arrayValues  
        { degree=degree;
        values=(Combinatorics.FisherYatesShuffle rnd idArray |> Seq.toArray)}

    let IsTwoCycle (perm:Permutation) =
        Combinatorics.isTwoCycle perm.values

    let CreateRandoms (degree:Degree) (rnd:IRando) =
        Seq.initInfinite(fun _ -> CreateRandom degree rnd)

    let InRange (degree:Degree) (value:int) =
       ((value > -1) && (value < (Degree.value degree)))

    let Inverse (p:Permutation) =
        create p.degree (Combinatorics.InverseMapArray (p |> arrayValues))
 
    let Product (pA:Permutation) (pB:Permutation) =
        if (Degree.value pA.degree) <> (Degree.value pB.degree) then
                Error (sprintf "degree %d <> degree %d:" 
                        (Degree.value pA.degree) (Degree.value pB.degree))
        else
            create pA.degree  (Combinatorics.ComposeMapIntArrays (pA |> arrayValues) (pB |> arrayValues))



 // a permutation of the set {0, 1,.. (degree-1)}, that is it's own inverse
type TwoCyclePerm = private {degree:Degree; values:int[] }
module TwoCyclePerm =

    let Identity (degree:Degree) = 
        {degree=degree; values=[|0 .. (Degree.value degree)-1|] }

    let arrayValues perm = perm.values
    let degree perm = perm.degree

    let Product (pA:TwoCyclePerm) (pB:TwoCyclePerm) =
        if (Degree.value pA.degree) <> (Degree.value pB.degree) then
                Error (sprintf "degree %d <> degree %d:" 
                        (Degree.value pA.degree) (Degree.value pB.degree))
        else
        { degree=pA.degree; 
          values= Combinatorics.ComposeMapIntArrays (pA |> arrayValues) (pB |> arrayValues)} |> Ok

    let MakeMonoCycle (degree:Degree) (hi:int) (low:int) =
        if ((Permutation.InRange degree hi) && (Permutation.InRange degree low)) then
            {degree=degree; 
             values=(Combinatorics.MakeMonoTwoCycle degree low hi)} |> Ok
        else Error "low or hi is out of range" 

    let MakeRandomMonoCycle (degree:Degree) (rnd:IRando) =
        { degree=degree; 
          values=Combinatorics.MakeRandomMonoTwoCycle degree rnd }

    let MakeAllMonoCycles (degree:Degree) =
        (Combinatorics.MakeAllMonoTwoCycles degree) 
        |> Seq.map (fun s -> {degree=degree; values= s})
     
    let MakeRandomFullTwoCycle (degree:Degree) (rnd:IRando) =
        { degree=degree; 
          values=Combinatorics.MakeRandomFullTwoCycleIntArray rnd (Degree.value degree)}

    let makeFromTupleSeq (degree:Degree) (tupes:seq<int*int>) =
        let curPa = [|0 .. (Degree.value degree)-1|]
        let validTupe t =
            ((fst t) <> (snd t)) &&
            (Degree.within degree (fst t)) &&
            (Degree.within degree (snd t))
        let usableTup t =
            (curPa.[fst(t)] = fst(t)) &&
            (curPa.[snd(t)] = snd(t))
        let OpPa tup =
            if (validTupe tup) && (usableTup tup) then
                curPa.[fst(tup)] <- snd(tup)
                curPa.[snd(tup)] <- fst(tup)

        tupes |> Seq.iter(OpPa)
        { degree=degree; values=curPa }


type BitArray = {order:int; items:array<bool>}
module BitArray =
    let Zero (order: int) =  { order=order; items=Array.init order (fun i -> false) }
    let Next (bits: BitArray) =  { order=bits.order; items=bits.items }

module ZeroOneSequence =

    let Random (rnd : IRando) (len: int) (pctOnes:float) =
        Seq.init len (fun n -> if (rnd.NextFloat > pctOnes) then 0 else 1)

    let FromInteger (len:int) (intVers:int) =
        let bitLoc (loc:int) (intBits:int) =
            if (((1 <<< loc) &&& intBits) <> 0) then 1 else 0
        Array.init len (fun i -> bitLoc i intVers)

    let ToInt (len:int) (arrayVers:int[]) =
        let mutable intRet = 0
        let bump i =
            if (arrayVers.[i] = 1) then
                intRet <- intRet + 1
            intRet <- intRet * 2

        {1 .. len} |> Seq.iter(fun i -> bump i)
        intRet


module IntBits =
    let Sorted_O_1_Sequence (blockLen:int) (onesCount:int) =
        seq {for i = 1 to blockLen - onesCount do yield 0; 
             for i = 1 to onesCount do yield 1 }

    //Returns a bloclLen + 1 length array of all possible sorted 0-1 sequences of length blockLen
    let Sorted_0_1_Sequences (blockLen:int) =
        seq {for i = 0 to blockLen 
                do yield (Sorted_O_1_Sequence blockLen i) |> Seq.toArray }
            |> Seq.toArray

    let AllBinaryTestCasesSeq (order:int) =
        {0 .. (1 <<< order) - 1}
        |> Seq.map (fun i -> ZeroOneSequence.FromInteger  order i)

    let AllBinaryTestCasesArray (order:int) =
        Array.init (1 <<< order) (fun i -> ZeroOneSequence.FromInteger order i)