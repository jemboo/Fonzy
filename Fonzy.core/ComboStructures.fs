namespace global


// a permutation of the set {0, 1,.. (degree-1)}
type Permutation = private {degree:Degree; values:int[] }
module Permutation =
    let create (degree:Degree) (vals:int[]) =
            {Permutation.degree=degree; values=vals }

    let createR (degree:Degree) (vals:int[]) =
        if vals.Length <> (Degree.value degree) then
            (sprintf "array length %d <> degree %d:" 
                      vals.Length (Degree.value degree)) |> Error 
        else
            create degree vals  |> Ok

    let identity (degree:Degree) = 
        {degree=degree; values=[|0 .. (Degree.value degree)-1|] }

    let rotate (degree:Degree) (dir:int) = 
        let d = (Degree.value degree)
        {degree=degree; values=Array.init d (fun i-> (i + dir) % d)}

    let arrayValues perm = perm.values
    let degree perm = perm.degree

    let areEqual (permA:Permutation) (permB:Permutation) =
        true

    let isTwoCycle (perm:Permutation) =
        Combinatorics.isTwoCycle perm.values

    let inRange (degree:Degree) (value:int) =
       ((value > -1) && (value < (Degree.value degree)))

    let inverse (p:Permutation) =
        create p.degree (Combinatorics.inverseMapArray (p |> arrayValues))

    let product (pA:Permutation) (pB:Permutation) =
        create pA.degree  (Combinatorics.composeMapIntArrays (pA |> arrayValues) (pB |> arrayValues))

    let productR (pA:Permutation) (pB:Permutation) =
        if (Degree.value pA.degree) <> (Degree.value pB.degree) then
                Error (sprintf "degree %d <> degree %d:" 
                        (Degree.value pA.degree) (Degree.value pB.degree))
        else
            product pA pB |> Ok

    let powers (maxPower:int) (perm:Permutation)  =
        let mutable loop = true
        let mutable curPerm = perm
        let mutable curPow = 0
        seq { while loop do 
                yield curPerm
                curPerm <- product perm curPerm
                curPow <- curPow + 1
                if ((curPerm = perm) || (curPow > maxPower)) then
                    loop <- false}

    let cyclicGroup (degree:Degree) = 
        let r1 = rotate degree 1
        powers ((Degree.value degree) - 1) r1 |> Seq.toList


    // IRando dependent

    let createRandom (degree:Degree) (rnd:IRando) =
        let idArray = (identity degree) |> arrayValues  
        { degree=degree;
        values=(Combinatorics.fisherYatesShuffle rnd idArray |> Seq.toArray)}

    let createRandoms (degree:Degree) (rnd:IRando) =
        Seq.initInfinite(fun _ -> createRandom degree rnd)



 // a permutation of the set {0, 1,.. (degree-1)}, that is it's own inverse
type TwoCyclePerm = private { degree:Degree; values:int[] }
module TwoCyclePerm =
    let create (degree:Degree) (values:int[]) = 
        if (Degree.value degree) <> values.Length then
            sprintf "array length %d <> degree %d:"  (Degree.value degree) 
                      values.Length |> Error 
        else { degree=degree; values=values } |> Ok

    let identity (degree:Degree) = 
        {degree=degree; values=[|0 .. (Degree.value degree)-1|] }

    let arrayValues perm = perm.values
    let degree perm = perm.degree

    let toPermutation (tcp:TwoCyclePerm) =
        { Permutation.degree = tcp.degree; 
          Permutation.values = tcp.values }

    let toTwoCycle (perm:Permutation) =
        if (Permutation.isTwoCycle perm) then
            { TwoCyclePerm.degree = perm.degree; 
              TwoCyclePerm.values = perm.values } |> Result.Ok
        else
            "Not a two cycle" |> Error

    let makeMonoCycle (degree:Degree) (hi:int) (low:int) =
        if ((Permutation.inRange degree hi) && (Permutation.inRange degree low)) then
            {degree=degree; 
             values=(Combinatorics.makeMonoTwoCycle degree low hi)} |> Ok
        else Error "low or hi is out of range" 

    let makeAllMonoCycles (degree:Degree) =
        (Combinatorics.makeAllMonoTwoCycles degree) 
        |> Seq.map (fun s -> {degree=degree; values= s})

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

    let makeEvenMode (degree:Degree) =
        let d = (Degree.value degree)
        let dm =
            if (d%2 > 0) then d-1
            else d
        let yak p =
            if p = dm then p
            else if (p%2 = 0) then
                 p + 1
            else p - 1
        { degree=degree; values=Array.init d (yak) }

    let makeOddMode (degree:Degree) =
        let d = (Degree.value degree)
        let dm =
            if (d%2 = 0) then d-1
            else d
        let yak p =
            if p = dm then p
            else if p = 0 then p
            else if (p%2 = 0) then
                 p - 1
            else p + 1
        { degree=degree; values=Array.init d (yak) }

    let makeAltEvenOdd (degree:Degree) =
        seq {while true do 
                    yield makeEvenMode degree; 
                    yield makeOddMode degree; }

    // IRando dependent
    
    let makeRandomMonoCycle (degree:Degree) (rnd:IRando) =
        { degree=degree; 
            values=Combinatorics.makeRandomMonoTwoCycle degree rnd }
        
    let makeRandomTwoCycle (degree:Degree) (rnd:IRando) (switchFreq:float) =
        let switchCount = Rando.multiDraw rnd switchFreq ((Degree.value degree) / 2)
        { degree=degree; 
            values=Combinatorics.makeRandomTwoCycleIntArray rnd (Degree.value degree) switchCount}
    
    let makeRandomFullTwoCycle (degree:Degree) (rnd:IRando) =
        { degree=degree; 
            values=Combinatorics.makeRandomFullTwoCycleIntArray rnd (Degree.value degree)}
    


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


type IntBits = { degree:Degree; values:int[] }
module IntBits =

    let Sorted_O_1_Sequence (blockLen:int) (onesCount:int) =
        seq {for i = 1 to blockLen - onesCount do yield 0; 
             for i = 1 to onesCount do yield 1 }

    //Returns a bloclLen + 1 length array 
    // of all possible sorted 0-1 sequences of length blockLen
    let Sorted_0_1_Sequences (blockLen:int) =
        seq {for i = 0 to blockLen 
                do yield (Sorted_O_1_Sequence blockLen i) |> Seq.toArray }
            |> Seq.toArray

    let AllBinaryTestCasesSeq (order:int) =
        {0 .. (1 <<< order) - 1}
        |> Seq.map (fun i -> ZeroOneSequence.FromInteger order i)

    let AllBinaryTestCasesArray (order:int) =
        Array.init (1 <<< order) (fun i -> ZeroOneSequence.FromInteger order i)


