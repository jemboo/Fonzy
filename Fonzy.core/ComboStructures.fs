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
        create pA.degree  (Combinatorics.composeMapIntArrays 
                                (pA |> arrayValues) 
                                (pB |> arrayValues))

    let conjugate (pA:Permutation) (conj:Permutation) =
        create pA.degree  (Combinatorics.conjugateIntArrays 
                                (pA |> arrayValues) 
                                (conj |> arrayValues))

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

    let product (pA:TwoCyclePerm) (pB:TwoCyclePerm) =
        create pA.degree  (Combinatorics.composeMapIntArrays 
                                (pA |> arrayValues) 
                                (pB |> arrayValues))

    let conjugate (pA:TwoCyclePerm) (conj:Permutation) =
        create pA.degree  (Combinatorics.conjugateIntArrays 
                                (pA |> arrayValues) 
                                (conj |> Permutation.arrayValues))

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

    // IRando dependent
    
    let makeRandomMonoCycle (degree:Degree) (rnd:IRando) =
        { degree=degree; 
            values=Combinatorics.makeRandomMonoTwoCycle degree rnd }
        
    let makeRandomTwoCycle (degree:Degree) (rnd:IRando) (switchFreq:float) =
        let switchCount = Rando.multiDraw rnd switchFreq ((Degree.value degree) / 2)
        { degree=degree; 
            values=Combinatorics.makeRandomTwoCycleIntArray 
                                    rnd 
                                    (Degree.value degree) 
                                    switchCount }
    
    let makeRandomFullTwoCycle (degree:Degree) (rnd:IRando) =
        { degree=degree; 
            values=Combinatorics.makeRandomFullTwoCycleIntArray 
                                            rnd 
                                            (Degree.value degree)}

    let reflect (twoCyclePerm:TwoCyclePerm) =
        let deg = (Degree.value twoCyclePerm.degree)
        let refV pos = Combinatorics.reflect deg
                                             pos
        let refl = Array.init 
                        deg
                        (fun dex -> 
         twoCyclePerm.values.[refV dex] |> refV)
        { degree = twoCyclePerm.degree; 
          values = refl }


    let makeReflSymmetric (degree:Degree) 
                          (rnd:IRando) =
        let deg = (Degree.value degree)
        let aRet = Array.init deg (id)
        let chunkProc (ch:(int*int)[]) =
            if ch.Length > 1 then
                let a,b = ch.[0]
                let c,d = ch.[1]
                aRet.[a] <- b
                aRet.[b] <- a
                aRet.[c] <- d
                aRet.[d] <- c
            else
               let a,b = ch.[0]
               aRet.[a] <- b
               aRet.[b] <- a

        let q = Combinatorics.reflectivePairs deg rnd
                |> Seq.toArray
        
        q |> Array.iter(chunkProc)
        { degree=degree; values=aRet }


module TwoCycleGen =
    let stack (lhs:int[]) (rhs:int[]) =
        let d = lhs.Length
        let aa = Array.init (d*2) (fun dex -> 
            if (dex < d) then lhs.[dex]
            else rhs.[dex-d] + d)
        aa

    let quad (qDex:int) (avs:int[])  =
        let qLen = avs.Length/4
        let qStart = qLen*qDex
        let qEnd = qStart + qLen - 1
        seq {for i=qStart to qEnd do yield avs.[i] }

    let t0 (avs:int[]) =
        avs

    let t1 (avs:int[]) =
        quad 3 avs
        |> Seq.append (quad 1 avs)
        |> Seq.append (quad 2 avs)
        |> Seq.append (quad 0 avs)
        |> Seq.toArray

    let t2 (avs:int[]) =
        quad 3 avs
        |> Seq.append (quad 0 avs)
        |> Seq.append (quad 1 avs)
        |> Seq.append (quad 2 avs)
        |> Seq.toArray

    let t3 (avs:int[]) =
        quad 0 avs
        |> Seq.append (quad 2 avs)
        |> Seq.append (quad 1 avs)
        |> Seq.append (quad 3 avs)
        |> Seq.toArray

    let t4 (avs:int[]) =
        quad 1 avs
        |> Seq.append (quad 2 avs)
        |> Seq.append (quad 3 avs)
        |> Seq.append (quad 0 avs)
        |> Seq.toArray

    let qStack (dex:int) (avsL:int[]) (avsR:int[]) =
        let avs = stack avsL avsR
        match dex with
        | 0 -> t0 avs
        | 1 -> t1 avs
        | 2 -> t2 avs
        | 3 -> t3 avs
        | _ -> t4 avs

    let qSeed (randy:IRando) =
        let rv = randy.NextPositiveInt % 2
        match rv with
        | 0 -> [|0;1|]
        | _ -> [|1;0|]

    let rndQstack (randy:IRando) =
        let q0 = qStack (randy.NextPositiveInt % 5)
                        (qSeed randy)
                        (qSeed randy)
        let q1 = qStack (randy.NextPositiveInt % 5)
                        (qSeed randy)
                        (qSeed randy)

        let q2 = qStack (randy.NextPositiveInt % 5)
                        (qSeed randy)
                        (qSeed randy)

        let q3 = qStack (randy.NextPositiveInt % 5)
                        (qSeed randy)
                        (qSeed randy)

        let h1 = qStack (randy.NextPositiveInt % 5)
                        q0
                        q1

        let h2 = qStack (randy.NextPositiveInt % 5)
                        q2
                        q3

        let res = qStack (randy.NextPositiveInt % 5)
                        h1
                        h2
        let conj = Combinatorics.composeMapIntArrays res
                        (Combinatorics.inverseMapArray res)
        {TwoCyclePerm.degree = Degree.fromInt 16; 
                    TwoCyclePerm.values = conj; }


    let evenDegree (degree:Degree) (offset:int) =
        let d = (Degree.value degree)
        let shoof v =
            if (v%2 = 0) then (v + d + offset) % d
            else (v + d - offset) % d
        let arr = Array.init d (shoof)
        {TwoCyclePerm.degree = degree; TwoCyclePerm.values = arr}
        

    let eightBlock0 (d:Degree) = 
        let _eightBlock0 (src:int) = 
            let mm = src % 8
            let mb = src - mm
            if mm = 0      then mb + 2
            else if mm = 1 then mb + 3
            else if mm = 2 then mb + 0
            else if mm = 3 then mb + 1
            else if mm = 4 then mb + 6
            else if mm = 5 then mb + 7
            else if mm = 6 then mb + 4
            else                mb + 5
        let aa = Array.init (Degree.value d) (_eightBlock0)
        { degree=d; values=aa }

    let eightBlock1 (d:Degree) = 
        let _eightBlock1 (src:int) = 
            let mm = src % 8
            let mb = src - mm
            if mm = 0      then mb + 4
            else if mm = 1 then mb + 5
            else if mm = 2 then mb + 6
            else if mm = 3 then mb + 7
            else if mm = 4 then mb + 0
            else if mm = 5 then mb + 1
            else if mm = 6 then mb + 2
            else                mb + 3
        let aa = Array.init (Degree.value d) (_eightBlock1)
        { degree=d; values=aa }

    let eightBlock2 (d:Degree) = 
        let _eightBlock2 (src:int) = 
            let mm = src % 8
            let mb = src - mm
            if mm = 0      then mb + 1
            else if mm = 1 then mb + 0
            else if mm = 2 then mb + 3
            else if mm = 3 then mb + 2
            else if mm = 4 then mb + 5
            else if mm = 5 then mb + 4
            else if mm = 6 then mb + 7
            else                mb + 6
        let aa = Array.init (Degree.value d) (_eightBlock2)
        { degree=d; values=aa }

    let make2EightBlocks (conj:Permutation list) =
        let coes (conj:Permutation) =
            result {
                        let! b0 = TwoCyclePerm.conjugate (eightBlock0 conj.degree) conj
                        let! b1 = TwoCyclePerm.conjugate (eightBlock1 conj.degree) conj
                        return seq { yield b0; yield b1; }
                   }
        result {
                    let! rOf = conj |> List.map(fun c -> coes c)
                                    |> Result.sequence
                    return rOf |> Seq.concat
               }

    let make3EightBlocks (conj:Permutation list) =
        let coes (conj:Permutation) =
            result {
                        let! b0 = TwoCyclePerm.conjugate (eightBlock0 conj.degree) conj
                        let! b1 = TwoCyclePerm.conjugate (eightBlock1 conj.degree) conj
                        let! b2 = TwoCyclePerm.conjugate (eightBlock2 conj.degree) conj
                        return seq { yield b0; yield b1; yield b2; }
                   }
        result {
                    let! rOf = conj |> List.map(fun c -> coes c)
                                    |> Result.sequence
                    return rOf |> Seq.concat
               }

    let fourBlock0 (d:Degree) = 
        let _fourBlock0 (src:int) = 
            let mm = src % 4
            let mb = src - mm
            if mm = 0 then mb + 1
            else if mm = 1 then mb
            else if mm = 2 then mb + 3
            else mb + 2
        let aa = Array.init (Degree.value d) (_fourBlock0)
        { degree=d; values=aa }

    let fourBlock1 (d:Degree)  = 
        let _fourBlock1 (src:int) = 
            let mm = src % 4
            let mb = src - mm
            if mm = 0 then mb + 3
            else if mm = 1 then mb + 2
            else if mm = 2 then mb + 1
            else mb
        let aa = Array.init (Degree.value d) (_fourBlock1)
        { degree=d; values=aa }

    let fourBlock2 (d:Degree) = 
        let _fourBlock2 (src:int) = 
            let mm = src % 4
            let mb = src - mm
            if mm = 0 then mb + 2
            else if mm = 1 then mb + 3
            else if mm = 2 then mb
            else mb + 1
        let aa = Array.init (Degree.value d) (_fourBlock2)
        { degree=d; values=aa }

    let make3FourBlocks (conj:Permutation list) =
        let coes (conj:Permutation) =
            result {
                        let! b0 = TwoCyclePerm.conjugate (fourBlock0 conj.degree) conj
                        let! b1 = TwoCyclePerm.conjugate (fourBlock1 conj.degree) conj
                        let! b2 = TwoCyclePerm.conjugate (fourBlock2 conj.degree) conj
                        return seq { yield b0; yield b1; yield b2; }
                   }
        result {
                    let! rOf = conj |> List.map(fun c -> coes c)
                                    |> Result.sequence
                    return rOf |> Seq.concat
               }

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
            else if p = 0 then 0
            else if (p%2 = 0) then
                 p - 1
            else p + 1
        { degree=degree; values=Array.init d (yak) }

    let makeOddModeFromEvenDegreeWithCap (degree:Degree) =
        let d = (Degree.value degree)
        let dm =
            if (d%2 = 0) then d-1
            else d
        let yak p =
            if p = 0 then d-1
            else if p = d-1 then 0
            else if (p%2 = 0) then
                 p - 1
            else p + 1
        { degree=degree; values=Array.init d (yak) }

    let makeOddModeWithCap (degree:Degree) =
        let d = (Degree.value degree)
        if (d%2 = 0) then makeOddModeFromEvenDegreeWithCap degree
        else makeOddMode degree

    let makeAltEvenOdd (degree:Degree) (conj:Permutation) =
        seq {while true do 
                    yield TwoCyclePerm.conjugate 
                            (makeEvenMode degree) conj; 
                    yield TwoCyclePerm.conjugate 
                            (makeOddModeWithCap degree) conj; }

    let makeCoConjugateEvenOdd (conj:Permutation list) =
        let coes (conj:Permutation) =
            result {
                        let! eve = TwoCyclePerm.conjugate 
                                    (makeEvenMode conj.degree) conj
                        let! odd = TwoCyclePerm.conjugate 
                                    (makeOddModeWithCap conj.degree) conj
                        return seq { yield eve; yield odd; }
                   }
        result {
                    let! rOf = conj |> List.map(fun c -> coes c)
                                    |> Result.sequence
                    return rOf |> Seq.concat
               }


type IntBits = { values:int[] }
module IntBits =
    let copy (intBits:IntBits) = 
        {IntBits.values = Array.copy (intBits.values) }

    let isZero (ibs:IntBits) = 
        ibs.values |> Array.forall((=) 0)

    let isSorted (intBits:IntBits) =
        Combinatorics.isSorted intBits.values

    let sorted_O_1_Sequence (degree:Degree) 
                            (onesCount:int) =
        let totalSize = (Degree.value degree)
        let numZeroes = totalSize - onesCount
        { IntBits.values = Array.init totalSize 
                    (fun i -> if i< numZeroes then 0 else 1)}

    //Returns a bloclLen + 1 length array of IntBits
    // of all possible sorted 0-1 sequences of length degree
    let sorted_0_1_Sequences (degree:Degree)  =
        seq { for i = 0 to (Degree.value degree) do 
                yield (sorted_O_1_Sequence degree i) }

    let randomPctOnes (rnd:IRando) 
                      (len:int) 
                      (pctOnes:float) =
        Seq.init len (fun _ -> 
                if (rnd.NextFloat > pctOnes) then 0 else 1)

    let fromInteger (len:int) (intVers:int) =
        let bitLoc (loc:int) (intBits:int) =
            if (((1 <<< loc) &&& intBits) <> 0) then 1 else 0
        { IntBits.values = 
                    Array.init len 
                               (fun i -> bitLoc i intVers) }

    let toInteger (arrayVers:IntBits) =
        let mutable intRet = 0
        let bump i =
            intRet <- intRet * 2
            if (arrayVers.values.[i] = 1) then
                intRet <- intRet + 1

        for i in (arrayVers.values.Length - 1) .. -1 .. 0 do
            bump i
        intRet

    let allBinary (degree:Degree) =
        let dv = Degree.value degree 
        {0 .. (1 <<< dv) - 1}
        |> Seq.map (fun i -> fromInteger dv i)

    let allBinaryArray (degree:Degree) =
        let order = (Degree.value degree)
        Array.init (1 <<< order) (fun i -> fromInteger order i)

    let createRandom (degree:Degree) (rando:IRando) = 
        let perm = Permutation.createRandom degree rando
        {IntBits.values = perm.values }

    let createRandoms (degree:Degree) (rnd:IRando) =
        seq { while true do 
                yield createRandom degree rnd }


type bitsP32 = { values:uint[] }
module bitsP32 =

    let zeroCreate (length:int) = 
        { bitsP32.values = 
                Array.create length 0u }

    let copy (pBits:bitsP32) = 
        {bitsP32.values = Array.copy (pBits.values) }

    let isZero (ibs:bitsP32) = 
        ibs.values |> Array.forall((=) 0u)

    let stripeWrite (uBits:bitsP32) 
                    (intBits:IntBits) 
                    (pos:int) = 
        let one = (1u <<< pos)
        let proc dex =
            if (intBits.values.[dex] = 1) then
                uBits.values.[dex] <- 
                            uBits.values.[dex] ||| one
        
        for i=0 to (uBits.values.Length - 1) do
            proc i


    let stripeRead (uBits:bitsP32) 
                   (pos:int) = 
        let one = (1u <<< pos)
        let proc dex v =
            if ((uBits.values.[dex] &&& one) > 0u) then
                1
            else 0
        { IntBits.values = uBits.values |> Array.mapi (proc) }


    let isSorted (uBits:bitsP32) =
        seq { 0 .. 31} |> Seq.map (fun pos -> stripeRead uBits pos)
           |> Seq.forall(IntBits.isSorted)


    let fromIntBits (ibSeq:IntBits seq) =
        seq { 
              use e = ibSeq.GetEnumerator()
              let nextChunk() =
                let res = zeroCreate e.Current.values.Length
                stripeWrite res e.Current 0
                let mutable i = 1
                while i < 32 && e.MoveNext() do
                    stripeWrite res e.Current i
                    i <- i + 1
                res

              while e.MoveNext() do
                yield nextChunk()  }


    let toIntBits (bp32s:bitsP32 seq) =
        seq { 
              use e = bp32s.GetEnumerator()
              let nextChunk bt32 =
                let mutable i = 0
                seq { 
                   while i < 32 do
                   let ibts =  stripeRead bt32 i
                   if (not (IntBits.isZero ibts)) then
                    yield ibts
                   i <- i + 1 }

              while e.MoveNext() do
                yield! nextChunk e.Current  }

    let allBinary (degree:Degree) =
        fromIntBits (IntBits.allBinary degree)

    let createRandoms (degree:Degree) (rnd:IRando) =
        (IntBits.createRandoms degree rnd) |> fromIntBits
