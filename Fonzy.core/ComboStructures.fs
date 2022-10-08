namespace global
open System.Numerics
open System

module IntSeries = 
    // returns true with an exp decreasing frequency
    let expoB (ticsPerLog:float) (value:int) = 
        if (ticsPerLog |> int) > value then
            true
        else 
            let logo = (ticsPerLog * Math.Log2 (value |> float)) |> int
            let logm = (ticsPerLog * Math.Log2 ((value - 1) |> float)) |> int
            logo > logm


    let logTics (ticsPerLog:float) (endVal:int) =
        seq {
            let mutable dex = 0
            while dex < endVal do
                if (expoB ticsPerLog dex) then yield dex
                dex <- dex + 1
        }


// a permutation of the set {0, 1,.. (degree-1)}
type permutation = private {degree:Degree; values:int[] }
module Permutation =
    let create (degree:Degree) (vals:int[]) =
            {permutation.degree=degree; values=vals }

    let createR (degree:Degree) (vals:int[]) =
        if vals.Length <> (Degree.value degree) then
            (sprintf "array length %d <> degree %d:" 
                      vals.Length (Degree.value degree)) |> Error 
        else
            create degree vals  |> Ok

    let identity (degree:Degree) = 
        {degree=degree; values= Combinatorics.identity (Degree.value degree)}

    let rotate (degree:Degree) (dir:int) = 
        let d = (Degree.value degree)
        {degree=degree; values=Array.init d (fun i-> (i + dir) % d)}

    let arrayValues perm = perm.values
    let degree perm = perm.degree

    let isTwoCycle (perm:permutation) =
        Combinatorics.isTwoCycle perm.values

    let inRange (degree:Degree) (value:int) =
       ((value > -1) && (value < (Degree.value degree)))

    let inverse (p:permutation) =
        create p.degree (Combinatorics.inverseMapArray (p |> arrayValues))

    let product (pA:permutation) (pB:permutation) =
        create pA.degree  (Combinatorics.composeIntArrayMaps 
                                (pA |> arrayValues) 
                                (pB |> arrayValues))

    let conjugate (pA:permutation) (conj:permutation) =
        create pA.degree  (Combinatorics.conjIntArrays 
                                (pA |> arrayValues) 
                                (conj |> arrayValues))

    let productR (pA:permutation) (pB:permutation) =
        if (Degree.value pA.degree) <> (Degree.value pB.degree) then
                Error (sprintf "degree %d <> degree %d:" 
                        (Degree.value pA.degree) (Degree.value pB.degree))
        else
            product pA pB |> Ok

    let powers (maxPower:int) (perm:permutation)  =
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
type twoCyclePerm = private { degree:Degree; values:int[] }
module TwoCyclePerm =
    let create (degree:Degree) (values:int[]) = 
        if (Degree.value degree) <> values.Length then
            sprintf "array length %d <> degree %d:"  (Degree.value degree) 
                      values.Length |> Error 
        else { degree=degree; values=values } |> Ok


    let identity (degree:Degree) = 
        {degree=degree; values= Combinatorics.identity (Degree.value degree)}


    let arrayValues perm = perm.values
    let degree perm = perm.degree


    let toPermutation (tcp:twoCyclePerm) =
        { permutation.degree = tcp.degree; 
          values = tcp.values }


    let product (pA:twoCyclePerm) (pB:twoCyclePerm) =
        create pA.degree  (Combinatorics.composeIntArrayMaps 
                                (pA |> arrayValues) 
                                (pB |> arrayValues))


    let conjugate (tcp:twoCyclePerm) (conj:permutation) =
        let tcpInts = (tcp |> arrayValues)
        let conjInts = (conj |> Permutation.arrayValues)
        create tcp.degree   (tcpInts
                             |> Combinatorics.conjIntArrays
                                conjInts)


    let toTwoCycle (perm:permutation) =
        if (Permutation.isTwoCycle perm) then
            { twoCyclePerm.degree = perm.degree; 
              values = perm.values } |> Result.Ok
        else
            "Not a two cycle" |> Error


    let makeMonoCycle (degree:Degree) (hi:int) (low:int) =
        if ((Permutation.inRange degree hi) && (Permutation.inRange degree low)) then
            {degree=degree; 
             values=(Combinatorics.makeMonoTwoCycle degree low hi)} |> Ok
        else Error "low or hi is out of range" 


    let reflect (tcp:twoCyclePerm) =
        let _refV pos = 
            Degree.reflect tcp.degree pos

        let refl = Array.init 
                    (Degree.value tcp.degree)
                    (fun dex -> tcp.values.[_refV dex] |> _refV)
        {
            degree = tcp.degree; 
            values = refl 
        }


    let hasAfixedPoint (tcp:twoCyclePerm) =
        tcp.values |> Seq.mapi(fun dex v -> dex = v)
                   |> Seq.contains(true)


    let isReflectionSymmetric (tcp:twoCyclePerm) =
        tcp = (reflect tcp)


    let isSymmetricTwoCycle (tcp:twoCyclePerm) =
        (isReflectionSymmetric tcp) &&
        (Combinatorics.isTwoCycle tcp.values)


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


    let mutateByPair (pair:int*int) 
                     (tcp:twoCyclePerm) =
        let tcpA = tcp |> arrayValues |> Array.copy
        let a, b = pair
        let c = tcpA.[a]
        let d = tcpA.[b]
        if (a=c) && (b=d) then
            tcpA.[a] <- b
            tcpA.[b] <- a
        elif (a=c) then
            tcpA.[a] <- b
            tcpA.[b] <- a
            tcpA.[d] <- d
        elif (b=d) then
            tcpA.[a] <- b
            tcpA.[b] <- a
            tcpA.[c] <- c
        else
            tcpA.[a] <- b
            tcpA.[c] <- d
            tcpA.[b] <- a
            tcpA.[d] <- c
        { tcp with values=tcpA}

        
    let mutateByReflPair (pairs: seq<(int*int)>) 
                         (tcp:twoCyclePerm) =
        //true if _mutato will always turn this into
        //another twoCyclePerm
        let _isMutatoCompatable (mut:twoCyclePerm) =
            (isSymmetricTwoCycle mut) &&
            not (hasAfixedPoint mut)

        let _mutato (pair:int*int) = 
            let tca = tcp |> arrayValues |> Array.copy
            let pA, pB = pair
            let tpA, tpB = tca.[pA], tca.[pB]
            let rA, rB = (pA |> Degree.reflect tcp.degree), (pB |> Degree.reflect tcp.degree)
            let rtA, rtB = (tpA |> Degree.reflect tcp.degree), (tpB |> Degree.reflect tcp.degree)

            tca.[pA] <- tpB
            tca.[tpB] <- pA

            tca.[pB] <- tpA
            tca.[tpA] <- pB

            tca.[rB] <- rtA
            tca.[rtA] <- rB

            tca.[rA] <- rtB
            tca.[rtB] <- rA

            { tcp with values=tca}

        //let muts =
        pairs |> Seq.map(fun pr -> _mutato pr)
                |> Seq.filter(_isMutatoCompatable)
                |> Seq.head
        //if muts.Length > 0 then
        //    muts.[0]
        //else
        //    { tcp with values = tcp |> arrayValues |> Array.copy}


    // IRando dependent
    
    let makeRandomMonoCycle (degree:Degree) (rnd:IRando) =
        { 
            degree = degree; 
            values = Combinatorics.rndMonoTwoCycle 
                                    degree 
                                    rnd 
        }
        
    let rndTwoCycle (degree:Degree)
                    (switchFreq:float) 
                    (rnd:IRando) =

        let _multiDraw (rnd:IRando) (freq:float) (numDraws:int)  =
            let __draw (randy:IRando) =
                if randy.NextFloat < freq then 1 else 0
            let mutable i=0
            let mutable successCount = 0
            while (i < numDraws) do
                    successCount <- successCount + __draw rnd
                    i <- i + 1
            successCount

        let switchCount = _multiDraw rnd 
                            switchFreq 
                            ((Degree.value degree) / 2)
        { 
           degree = degree; 
           values = Combinatorics.rndTwoCycleArray 
                            rnd 
                            (Degree.value degree) 
                            switchCount 
         }
    

    let rndFullTwoCycle (degree:Degree) 
                        (rnd:IRando) =
        { 
            degree = degree; 
            values = Combinatorics.rndFullTwoCycleArray 
                            rnd 
                            (Degree.value degree)
        }

    let rndSymmetric (degree:Degree) 
                     (rnd:IRando) =
        let deg = (Degree.value degree)
        let aRet = Array.init deg (id)
        let chunkRi (rfls:switchRfl) =
            match rfls with
            | Single (i, j, d)         ->  aRet.[i] <- j
                                           aRet.[j] <- i

            | Unreflectable (i, j, d)  ->  aRet.[i] <- j
                                           aRet.[j] <- i

            | Pair ((h, i), (j, k), d) ->  aRet.[i] <- h
                                           aRet.[h] <- i
                                           aRet.[j] <- k
                                           aRet.[k] <- j

            | LeftOver (i, j, d)       ->  aRet.[i] <- j
                                           aRet.[j] <- i

        let q = SwitchRfl.rndReflectivePairs degree rnd
                |> Seq.iter(chunkRi)

        { degree=degree; values=aRet }



module TwoCycleGen =

    let evenMode (degree:Degree) =
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


    let oddMode (degree:Degree) =
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


    let oddModeFromEvenDegreeWithCap (degree:Degree) =
        let d = (Degree.value degree)
        let yak p =
            if p = 0 then d-1
            else if p = d-1 then 0
            else if (p%2 = 0) then
                 p - 1
            else p + 1
        { degree=degree; values=Array.init d (yak) }


    let oddModeWithCap (degree:Degree) =
        let d = (Degree.value degree)
        if (d%2 = 0) then oddModeFromEvenDegreeWithCap degree
        else oddMode degree


    let makeAltEvenOdd (degree:Degree) (conj:permutation) =
        seq {while true do 
                yield TwoCyclePerm.conjugate 
                        (evenMode degree) conj; 
                yield TwoCyclePerm.conjugate 
                        (oddModeWithCap degree) conj; }


    let makeCoConjugateEvenOdd (conj:permutation list) =
        let coes (conj:permutation) =
            result {
                    let! eve = TwoCyclePerm.conjugate 
                                (evenMode conj.degree) conj
                    let! odd = TwoCyclePerm.conjugate 
                                (oddModeWithCap conj.degree) conj
                    return seq { yield eve; yield odd; }
                   }
        result {
                    let! rOf = conj |> List.map(fun c -> coes c)
                                    |> Result.sequence
                    return rOf |> Seq.concat
               }


type intSet = { values:int[] }
module IntSet =

    let create (avs:int[]) = 
        {intSet.values = avs}

    let zeroCreate (count:int) = 
        { intSet.values = 
                Array.create count 0 }

    let copy (intSet:intSet) = 
        {intSet.values = Array.copy (intSet.values) }

    let isZero (ibs:intSet) = 
        ibs.values |> Array.forall((=) 0)

    let isSorted (intSet:intSet) =
        Combinatorics.isSortedI intSet.values

    let fromInteger (len:int) (intVers:int) =
        let bitLoc (loc:int) (intBits:int) =
            if (((1 <<< loc) &&& intBits) <> 0) then 1 else 0
        { intSet.values = 
                    Array.init len 
                                (fun i -> bitLoc i intVers) }

    let toInteger (arrayVers:intSet) =
        let mutable intRet = 0
        let bump i =
            intRet <- intRet * 2
            if (arrayVers.values.[i] = 1) then
                intRet <- intRet + 1

        for i in (arrayVers.values.Length - 1) .. -1 .. 0 do
            bump i
        intRet
                
    let fromUint64 (len:int) (intVers:int) =
        let bitLoc (loc:int) (intBits:int) =
            if (((1 <<< loc) &&& intBits) <> 0) then 1 else 0
        { intSet.values = 
                    Array.init len 
                                (fun i -> bitLoc i intVers) }
                
                
    let toUint64 (arrayVers:intSet) =
        let mutable intRet = 0UL
        let bump i =
            intRet <- intRet * 2UL
            if (arrayVers.values.[i] = 1) then
                intRet <- intRet + 1UL
                
        for i in (arrayVers.values.Length - 1) .. -1 .. 0 do
            bump i
        intRet


    let seqOfAllFor (degree:Degree) =
        let dv = Degree.value degree 
        {0 .. (1 <<< dv) - 1}
        |> Seq.map (fun i -> fromInteger dv i)


    let arrayOfAllFor (degree:Degree) =
        let order = (Degree.value degree)
        Array.init (1 <<< order) (fun i -> fromInteger order i)


    let createRandom (degree:Degree) (rando:IRando) = 
        let perm = 
            Array.init (Degree.value degree)
                        (fun _ -> let q = rando.NextFloat
                                  if (q > 0.5) then 1 else 0 )
        {intSet.values = perm }


    let createRandoms (degree:Degree) 
                        (rnd:IRando) =
        seq { while true do 
                yield createRandom degree rnd }


type bitSet = { values:int[] }
module BitSet =

    let create (avs:int[]) = 
        {bitSet.values = avs}

    let zeroCreate (count:int) = 
        { bitSet.values = 
                Array.create count 0 }

    let copy (bitSet:bitSet) = 
        {bitSet.values = Array.copy (bitSet.values) }

    let fromIntSet (ntSet:intSet) =
        create ntSet.values

    let toIntSet (btSet:bitSet) = 
        IntSet.create btSet.values

    let isZero (ibs:bitSet) = 
        ibs.values |> Array.forall((=) 0)

    let isSorted (bitSet:bitSet) =
        Combinatorics.isSortedI bitSet.values

    let sorted_O_1_Sequence (degree:Degree) 
                            (onesCount:int) =
        let totalSize = (Degree.value degree)
        let numZeroes = totalSize - onesCount
        { bitSet.values = Array.init totalSize 
                    (fun i -> if i< numZeroes then 0 else 1)}

    //Returns a bloclLen + 1 length array of IntBits
    // of all possible sorted 0-1 sequences of length degree
    let sorted_0_1_Sequences (degree:Degree)  =
        seq { for i = 0 to (Degree.value degree) do 
                yield (sorted_O_1_Sequence degree i) }


    let stack (lowTohi: bitSet seq) =
        lowTohi |> Seq.map(fun bs->bs.values)
                |> Seq.concat
                |> Seq.toArray
                |> create

    let comboStack (subSeqs: bitSet[] seq) =
        let rec _cart LL =
            match LL with
            | [] -> Seq.singleton []
            | L::Ls -> seq {for x in L do for xs in _cart Ls -> x::xs}
        _cart (subSeqs |> Seq.toList) |> Seq.map(stack)


    let stackSortedBlocks (blockSizes:Degree seq) =
        blockSizes |> Seq.map(sorted_0_1_Sequences >> Seq.toArray)
                   |> comboStack


    let fromInteger (len:int) (intVers:int) =
        let bitLoc (loc:int) (intBits:int) =
            if (((1 <<< loc) &&& intBits) <> 0) then 1 else 0
        { bitSet.values = 
                    Array.init len 
                               (fun i -> bitLoc i intVers) }



    let toInteger (arrayVers:bitSet) =
        let mutable intRet = 0
        let bump i =
            intRet <- intRet * 2
            if (arrayVers.values.[i] = 1) then
                intRet <- intRet + 1

        for i in (arrayVers.values.Length - 1) .. -1 .. 0 do
            bump i
        intRet

                
    let fromUint64 (len:int) (intVers:int) =
        let bitLoc (loc:int) (intBits:int) =
            if (((1 <<< loc) &&& intBits) <> 0) then 1 else 0
        { bitSet.values = 
                    Array.init len 
                                (fun i -> bitLoc i intVers) }
                
                
    let toUint64 (arrayVers:bitSet) =
        let mutable intRet = 0UL
        let bump i =
            intRet <- intRet * 2UL
            if (arrayVers.values.[i] = 1) then
                intRet <- intRet + 1UL
                
        for i in (arrayVers.values.Length - 1) .. -1 .. 0 do
            bump i
        intRet


    let seqOfAllFor (degree:Degree) =
        let dv = Degree.value degree 
        {0 .. (1 <<< dv) - 1}
        |> Seq.map (fun i -> fromInteger dv i)


    let arrayOfAllFor (degree:Degree) =
        let order = (Degree.value degree)
        Array.init (1 <<< order) (fun i -> fromInteger order i)


    let createRandom (degree:Degree) (rando:IRando) = 
        let perm = 
            Array.init (Degree.value degree)
                       (fun _ -> let q = rando.NextFloat
                                 if (q > 0.5) then 1 else 0   )
        {bitSet.values = perm }


    let createRandoms (degree:Degree) 
                      (rnd:IRando) =
        seq { while true do 
                yield createRandom degree rnd }



type bitsP32 = { values:uint[] }
module BitsP32 =

    let zeroCreate (count:int) = 
        { bitsP32.values = 
                Array.create count 0u }


    let zeroSubCreate (count:int) = 
        { bitsP32.values = 
                Array.create ((count + 31) / 32) 0u }


    let copy (pBits:bitsP32) = 
        { bitsP32.values = Array.copy (pBits.values) }


    let isZero (ibs:bitsP32) = 
        ibs.values |> Array.forall((=) 0u)

    let stripeWrite (uBits:bitsP32) 
                    (bitSet:bitSet) 
                    (pos:int) = 
        let one = (1u <<< pos)
        let proc dex =
            if (bitSet.values.[dex] = 1) then
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
        { bitSet.values = uBits.values |> Array.mapi (proc) }


    let isSorted (uBits:bitsP32) =
        seq { 0 .. 31} |> Seq.map (fun pos -> stripeRead uBits pos)
           |> Seq.forall(BitSet.isSorted)


    let fromBitSets (ibSeq:bitSet seq) =
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


    let toBitSets (bp32s:bitsP32 seq) =
        seq { 
              use e = bp32s.GetEnumerator()
              let nextChunk bt32 =
                let mutable i = 0
                seq { 
                   while i < 32 do
                   let ibts =  stripeRead bt32 i
                   if (not (BitSet.isZero ibts)) then
                    yield ibts
                   i <- i + 1 }

              while e.MoveNext() do
                yield! nextChunk e.Current  }


    let seqOfAllFor (degree:Degree) =
        fromBitSets (BitSet.seqOfAllFor degree)


    let arrayOfAllFor (degree:Degree) =
        fromBitSets (BitSet.arrayOfAllFor degree)
        |> Seq.toArray


    let createRandoms (degree:Degree) 
                      (rnd:IRando) 
                      (count:int) =
        (BitSet.createRandoms degree rnd) 
            |> Seq.take count
            |> fromBitSets



type bitsP64 = { values: uint64[] }
module BitsP64 =

    let zeroCreate (count:int) = 
        { bitsP64.values = 
                Array.create count 0UL }

    let pBlocksFor (count:int) = 
        ((count + 63) / 64)

    let copy (pBits:bitsP64) = 
        { bitsP64.values = Array.copy (pBits.values) }

    let isZero (ibs:bitsP64) = 
        ibs.values |> Array.forall((=) 0UL)

    let stripeWrite (uBits:bitsP64) 
                    (bitSet:bitSet) 
                    (pos:int) =
        ByteUtils.stripeWrite uBits.values
                              bitSet.values
                              pos

    let stripeRead (uBits:bitsP64) 
                   (pos:int) = 
        { bitSet.values = ByteUtils.stripeRead uBits.values pos }


    let isSorted (uBits:bitsP64) =
        seq { 0 .. 63 } |> Seq.map (fun pos -> stripeRead uBits pos)
           |> Seq.forall(BitSet.isSorted)


    let fromBitSet (ibSeq:bitSet seq) =
        seq { 
              use e = ibSeq.GetEnumerator()
              let _nextChunk() =
                let res = zeroCreate e.Current.values.Length
                stripeWrite res e.Current 0
                let mutable i = 1
                while i < 64 && e.MoveNext() do
                    stripeWrite res e.Current i
                    i <- i + 1
                res

              while e.MoveNext() do yield _nextChunk()  
            }


    let fromIntSet (ibSeq:intSet seq) =
        seq { 
              use e = ibSeq.GetEnumerator()
              let _nextChunk() =
                let res = zeroCreate e.Current.values.Length
                stripeWrite res (e.Current |> BitSet.fromIntSet) 0
                let mutable i = 1
                while i < 64 && e.MoveNext() do
                    stripeWrite res (e.Current |> BitSet.fromIntSet) i
                    i <- i + 1
                res

              while e.MoveNext() do yield _nextChunk()  
            }


    // returns only the nonzero sets
    let toBitSets (bp64s:bitsP64 seq) =
        seq { 
              use e = bp64s.GetEnumerator()
              let nextChunk bt64 =
                let mutable i = 0
                seq { 
                   while i < 64 do
                   let ibts =  stripeRead bt64 i
                   if (not (BitSet.isZero ibts)) then
                    yield ibts
                   i <- i + 1 }

              while e.MoveNext() do
                yield! nextChunk e.Current  }


    // returns only the nonzero sets
    let toIntSets (bp64s:bitsP64 seq) =
        bp64s |> toBitSets |> Seq.map(BitSet.toIntSet)


    let seqOfAllFor (degree:Degree) =
        fromBitSet (BitSet.seqOfAllFor degree)


    let arrayOfAllFor (degree:Degree) =
        fromBitSet (BitSet.arrayOfAllFor degree)
        |> Seq.toArray


    let createRandoms (degree:Degree) 
                      (rnd:IRando) 
                      (count:int) =
        (BitSet.createRandoms degree rnd) 
            |> Seq.take count
            |> fromBitSet



type record64Array = { values: uint64[] }
module Record64Array =

    let make (degree:Degree) = 
        let arrayLen = 1 <<< ( (Degree.value degree) - 6 )
        {record64Array.values = Array.zeroCreate<uint64> arrayLen }


    let recordPosition (records:record64Array) (pos:uint64) = 
        let bitPos = pos % 64UL |> int
        let recordPos = pos >>> 6 |> int
        let stamp = 1UL <<< bitPos
        let record = records.values.[recordPos] ||| stamp
        records.values.[recordPos] <- record


    let recordIntBits (records:record64Array) (bitSet:bitSet) = 
        let pos = bitSet |> BitSet.toInteger
        let bitPos = pos % 64
        let recordPos = pos >>> 6 |> int
        let stamp = 1UL <<< bitPos
        let record = records.values.[recordPos] ||| stamp
        records.values.[recordPos] <- record


    let toBitSets (degree:Degree) (records:record64Array)= 
        seq {
                for i in 0 .. ( records.values.Length - 1 ) do
                    yield! ( records.values.[i] 
                                |> ByteUtils.trueBitIndexes64 
                                |> Seq.map(fun dx -> dx + i*64 ) )
        }  |> Seq.map(BitSet.fromInteger (Degree.value degree))


type vecP64 = { values: uint64[] }

type vecP64b = { values: Vector<uint64> }

module VecP64 = 
    
    //let makeVec (ofw:vecP64) = 
    //    let lowVec = Vector(ofw.values)

    //    None
    
    //let fromBitsP64 (degree:Degree)
    //                (bitsP64:seq<bitsP64>) =
    //    let fromRz (rz:ResizeArray<uint64>) = 
    //        Vector( rz |> Seq.toArray)
            
    //    let rszFv = Array.init (Degree.value degree)
    //                           (fun _ -> new ResizeArray<uint64>())
    //    let rszAppend (bp:bitsP64) = 
    //        bp.values |> Array.iteri(fun i ui64 -> rszFv.[i].Add ui64)

    //    bitsP64 |> Seq.iter(rszAppend)
    //    {
    //        sortableVecb.degree = degree;
    //        vecLines = 
    //            Array.init
    //                (Degree.value degree)
    //                (fun i -> fromRz rszFv.[i] )
    //    }


    //let fromIntBits (degree:Degree)
    //                (intBits:seq<IntBits>) =
    //    intBits |> BitsP64.fromIntBits
    //            |> fromBitsP64 degree


    let aOr (lhs:uint64[]) 
            (rhs:uint64[]) 
            (res:uint64[]) =
        let mutable i = 0
        while i < lhs.Length - 3 do
            let vlhs = Vector<uint64>(lhs, i)
            let vrhs = Vector<uint64>(rhs, i)
            let vres = vlhs ||| vrhs
            vres.CopyTo(res, i)
            i <- i + 4
        while i < lhs.Length do
            res.[i] <- lhs.[i] ||| rhs.[i]
            i <- i + 1


    let aOr2 (lhs:uint64[]) 
             (rhs:uint64[]) 
             (res:uint64[]) =
        let mutable i = 0
        while i < lhs.Length do
            res.[i] <- lhs.[i] ||| rhs.[i]
            i <- i + 1
       
