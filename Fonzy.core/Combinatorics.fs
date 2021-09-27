namespace global
open System

module Combinatorics =

    let identity (degree:int) =
        [|0 .. degree-1|] 


    let composeMapIntArrays (a:array<int>) 
                            (b:array<int>) =
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
    let conjugateIntArrays (a:array<int>) 
                           (conj:array<int>) =
        composeMapIntArrays conj (composeMapIntArrays a (inverseMapArray conj) )

    let isSorted (values:int seq) =
        if values |> Seq.isEmpty then
            true
        else
            let yak = values.GetEnumerator()
            yak.MoveNext() |> ignore
            let mutable lastVal = yak.Current
            let mutable cont = true
            while (yak.MoveNext() && cont) do
                if (yak.Current < lastVal) then
                    cont <- false
                else
                    lastVal <- yak.Current
            cont

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

    let isSortedOffsetU (baseValues:uint[]) (offset:int) (length:int) =
        let mutable i=1
        let mutable looP = true
        while ((i < length) && looP) do
             looP <- (baseValues.[i+offset-1] <= baseValues.[i+offset])
             i<-i+1
        looP

    let isSortedOffsetUL (baseValues:uint64[]) (offset:int) (length:int) =
        let mutable i=1
        let mutable looP = true
        while ((i < length) && looP) do
             looP <- (baseValues.[i+offset-1] <= baseValues.[i+offset])
             i<-i+1
        looP

    let isTwoCycle (a:int[]) =
        (composeMapIntArrays a a) = identity a.Length

    let fixedCount (a:int[]) =
        a |> Array.mapi(fun dex e -> if (dex = e) then 1 else 0)
          |> Array.reduce(+)

    let distanceSquared (a:array<int>) 
                        (b:array<int>) =
        Array.fold2 (fun acc elem1 elem2 ->
        acc + (elem1 - elem2) * (elem1 - elem2)) 0 a b

    // Measured in bits (log base 2)
    let entropyBits (a:array<int>) =
        let f = 1.0 / Math.Log(2.0)
        let tot = float (a |> Array.sum)
        let fa = a  |> Array.filter(fun i->i>0)
                    |> Array.map (fun i->(float i) / tot)
        let res = Array.fold (fun acc elem -> 
                        acc - elem * f * Math.Log(elem)) 0.0 fa
        res 

    let unsortednessSquared (a:array<int>) =
        distanceSquared a [|0 .. (a.Length - 1)|]

    let makeMonoTwoCycle (degree:Degree) 
                         (aDex:int) 
                         (bDex:int) =
        Array.init (Degree.value degree) (fun i -> 
            if   (i = aDex) then bDex
            elif (i = bDex) then aDex
            else i)

    let makeAllMonoTwoCycles (degree:Degree) =
        seq {for i = 0 to (Degree.value(degree) - 1) do
                for j = 0 to i - 1 do
                    yield makeMonoTwoCycle degree i j}

    // bins is an increasing set of positive numbers.
    // returns the index of the first member e>value.
    let findBin (bins:float[]) 
                (value:float) =
        bins |> Array.findIndex(fun b -> b>value)

    // converts a density distr to a cumulative distr.
    let cumSum (startingVal:float) 
               (weights:float[]) =
        let mutable tot = startingVal
        let cumo w =
            tot<-tot + w
            tot
        weights |> Array.map(fun w -> cumo w)
        
    let makeHull (seqX:seq<'a>) (seqY:seq<'a>) (x:int) (y:int) =
        let xa = seqX |> Seq.take(x) |> Seq.toArray
        let ya = seqY |> Seq.take(y) |> Seq.toArray
        seq {for i = 0 to x - 1 do yield (xa.[i], ya.[y-1])}
            |> Seq.append (seq {for i = 0 to y - 2 do yield (xa.[x-1], ya.[i])})

    let randOneOrZero (pctOnes:float) (rnd:IRando) 
                      (len:int) =
        Seq.init len (fun _ -> 
                if (rnd.NextFloat > pctOnes) then 0 else 1)

    let drawTwoWithoutRep (degree:Degree) 
                          (rnd:IRando) =
        let aBit = rnd.NextPositiveInt % Degree.value(degree)
        let mutable bBit = rnd.NextPositiveInt % Degree.value(degree)
        while aBit = bBit do
            bBit <- rnd.NextPositiveInt % Degree.value(degree)
        if aBit < bBit then aBit, bBit
        else bBit, aBit

    let rndMonoTwoCycle (degree:Degree) (rnd:IRando) =
        let tup = drawTwoWithoutRep degree rnd
        makeMonoTwoCycle degree (fst tup) (snd tup)


    let draw1D (min:float) (max:float) (rnd:IRando) =
        seq { while true do 
                yield min + rnd.NextFloat * (max - min) }


    let draw2D (min1:float) (max1:float)
               (min2:float) (max2:float) 
               (rnd:IRando) =
        seq { while true do 
                yield ( min1 + rnd.NextFloat * (max1 - min1),
                        min2 + rnd.NextFloat * (max2 - min2)) }


    let drawFromWeightedDistribution (weightFunction:float->float) 
                                     (rnd:IRando) 
                                     (items:float[]) =
        let bins = items |> Array.map(weightFunction)
                         |> cumSum 0.0
        let maxVal = bins.[bins.Length - 1]
        Seq.initInfinite(fun _-> items.[findBin bins (rnd.NextFloat * maxVal)])

    // returns a sequence of draws from initialList without replacement. 
    // Does not change initialList
    let fisherYatesShuffle (rnd:IRando) 
                           (initialList:array<'a>) =
        let rndmx max = rnd.NextUInt % max
        let availableFlags = Array.init initialList.Length 
                              (fun i -> (i, true))
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


    let reflect (degree:int) (src:int) =
        degree - src - 1

    let randomPermutation (rnd:IRando) 
                          (degree:int) =
         (fisherYatesShuffle rnd)  [|0 .. degree-1|] |> Seq.toArray


    let randomPermutations (rnd:IRando) 
                           (degree:int) =
         Seq.initInfinite (fun _ -> randomPermutation rnd degree)


    let rndTwoCycleArray (rnd:IRando) 
                         (arraysize:int) 
                         (cycleCount:int) =
        let initialList = [|0 .. arraysize-1|]
        let arrayRet = Array.init arraysize (fun i -> i)
        let rndTupes = (fisherYatesShuffle rnd initialList) 
                       |> (Seq.chunkBySize 2) |> Seq.toArray
        for i = 0 to cycleCount - 1 do
            arrayRet.[rndTupes.[i].[0]] <- rndTupes.[i].[1]
            arrayRet.[rndTupes.[i].[1]] <- rndTupes.[i].[0]
        arrayRet


    let rndFullTwoCycleArray (rnd:IRando) 
                             (arraysize:int) =
        rndTwoCycleArray rnd arraysize (arraysize/2)


    let locsPosArrayo (arrayLen:int) (locs: (int*int*int) list) = 
        let arrayRet = Array.zeroCreate arrayLen
        locs |> List.iter (fun (l, loc, u) -> arrayRet.[loc] <- 1)
        arrayRet

    let locsPosArray (arrayLen:int) (locs: int list) = 
        let arrayRet = Array.zeroCreate arrayLen
        locs |> List.iter (fun loc -> arrayRet.[loc] <- 1)
        arrayRet

    let enumNchooseM (n:int) 
                     (m:int) =
        
        let maxVal = n - 1

        let newMaxf l r =  
            let rh, rt = 
                match r with
                | rh::rt ->  (rh, rt)
                | [] -> failwith  "boo boo"
            rh + 2 + ( l |> List.length )

        let rightPack l r = 
            let rh, rt = 
                match r with
                | rh::rt ->  (rh, rt)
                | [] -> failwith  "boo boo"
            let curMax = newMaxf l r
            let rhS = rh + 1
            if (curMax = maxVal) then 
              [(rhS + 1) .. maxVal], rhS, rt
                else 
              [], curMax, [(curMax - 1) .. -1 .. rhS]@rt

        let rec makeNext (lhs:int list) 
                         (c:int)  
                         (rhs:int list) =
            let maxShift = match lhs with
                            | a::_ -> a - 1
                            | _ -> maxVal
            match lhs, c, rhs with
            | l, md, [] when md = maxShift -> None
            | l, md, r when md < maxShift -> Some (l, md + 1, r) 
            | l, md, rh::rt when (md = maxShift ) && 
                                 (rh = (maxShift - 1)) -> 
                        makeNext (md::l) rh  rt
            | l, md, r when (md = maxShift ) -> Some (rightPack l r)
            | l, md, r when md = maxShift -> Some (md::l, md + 1, r) 
            | _, _, _ -> None

        let mutable proceed = true
        let mutable curTup = Some ([], m - 1, [(m - 2) .. -1 .. 0])
        seq { while proceed do
                  let a, b, c = curTup |> Option.get
                  yield a@(b::c) |> List.sort 
                  curTup <- makeNext a b c
                  proceed <- (curTup |> Option.isSome)
            }

    let capN (n:int)
             (cap:int) 
             (srcA:int[]) =
        seq { for i in 0 .. (n-1) do
              if (srcA.[i] < cap) then
                yield i }
            |> Seq.toList

             
    let rndNchooseM (n:int) 
                    (m:int) 
                    (rnd:IRando) =
        randomPermutations rnd n
        |> Seq.map(capN n m)


    let mapSubset (degree:Degree)
                  (subset: int list)  =
        let aRet = Array.create (Degree.value degree)
                                 None 
        subset |> List.iteri(fun dex dv -> aRet.[dv] <- Some dex)
        aRet




type reflectiveIndexes =
     | Single of int*int*Degree
     | Unreflectable of int*int*Degree
     | Pair of (int*int)*(int*int)*Degree
     | LeftOver of int*int*Degree

module ReflectiveIndexes =

    let reflect (degree:int) (src:int) =
        degree - src - 1


    let isReflSymmetric (degree:int) (pair:int*int) =
        (reflect degree (fst pair)) = (snd pair)


    let getIndexes (rfls:reflectiveIndexes) =
        match rfls with
        | Single (i, j, d)         ->  seq { i; j; }
        | Unreflectable (i, j, d)  ->  seq { i; j; }
        | Pair ((h, i), (j, k), d) ->  seq { h; i; j; k; }
        | LeftOver (i, j, d)       ->  seq { i; j; }


    let isGood (rfls:reflectiveIndexes) =
        match rfls with
        | Single _         ->  true
        | Unreflectable _  ->  false
        | Pair _           ->  true
        | LeftOver _       ->  false


    let isAFullSet (degree:Degree) 
                   (rflses:reflectiveIndexes seq) = 
        let dexes = rflses |> Seq.map(getIndexes)
                           |> Seq.concat
                           |> Seq.sort
                           |> Seq.toList
        [1 .. ((Degree.value degree) - 1)] = dexes


    let reflectivePairs (degree:Degree)
                        (rnd:IRando) =
            let _rndmx max = 
                (int rnd.NextPositiveInt) % max
            let _reflectD (dex:int) =
                reflect (Degree.value degree) dex

            let _flagedArray = 
                Array.init (Degree.value degree)
                           (fun i -> (i, true))

            let _availableFlags() =
                _flagedArray                         
                |> Seq.filter (fun (ndx,f) -> f)

            let _canContinue() =
                _availableFlags() |> Seq.length > 1

            let _nextItem() =
                let nItem = _rndmx (_availableFlags() |> Seq.length)     
                let index =                          
                    _availableFlags()
                    |> Seq.item (int nItem)             
                    |> fst   
                _flagedArray.[index] <- (index, false) 
                index

            let _getReflection (a:int) (b:int) =
                let aR = _reflectD a
                let bR = _reflectD b
                if (snd _flagedArray.[aR]) && (snd _flagedArray.[bR]) then
                    _flagedArray.[aR] <- (aR, false) 
                    _flagedArray.[bR] <- (bR, false) 
                    Some (bR, aR)
                else    
                    None

            let _nextItems() =
                let nItemA = _nextItem()
                let nItemB = _nextItem()
                if nItemA = (_reflectD nItemB) then
                   (nItemA, nItemB, degree) |> reflectiveIndexes.Single
                // if one of the nodes is on the center line, then make a (non-reflective) 
                // pair out of them
                else if (nItemA = (_reflectD nItemA)) || 
                        (nItemB = (_reflectD nItemB)) then
                    (nItemA, nItemB, degree) |> reflectiveIndexes.Unreflectable
                else
                    let res = _getReflection nItemA nItemB
                    match res with
                    | Some (reflA, reflB) ->
                              ((nItemA, nItemB), (reflA, reflB), degree) 
                                    |> reflectiveIndexes.Pair
                    // if a reflective pair cannot be made from these two, then
                    // make them into a (non-reflective) pair
                    | None -> (nItemA, nItemB, degree) |> reflectiveIndexes.LeftOver

            seq { while _canContinue() do yield _nextItems() }

    // the reflectivePairs function above generates only good 
    // reflective pairs for even degree
    let goodReflectivePairs (degree:Degree)
                            (rnd:IRando) =
         seq { 
            while true do
                 let rfpa = reflectivePairs degree rnd          
                            |> Seq.toArray
                 if (rfpa |> Array.forall (isGood)) then
                    yield rfpa
              }



