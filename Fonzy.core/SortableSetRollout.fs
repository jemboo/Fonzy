namespace global
open System

type intSetsRollout = 
        {   degree:Degree; 
            baseArray:int[]; 
            sortableCount:SortableCount  }


type bP64SetsRollout = 
        {   degree:Degree; 
            baseArray:uint64[]; 
            sortableCount:SortableCount  }


type sortableSetRollout =
    | Int of intSetsRollout
    | Bp64 of bP64SetsRollout


module IntSetsRollout =

    let create (degree:Degree) (baseArray:int[]) =
        if baseArray.Length < 0 + (Degree.value degree) then
            Error (sprintf "baseArray length %d is not a multiple of degree: %d:" 
                    baseArray.Length (Degree.value degree))
        else
            let baseCopy = Array.zeroCreate baseArray.Length
            Array.Copy(baseArray, baseCopy, baseArray.Length)
            {
                intSetsRollout.degree = degree; 
                baseArray = baseCopy; 
                sortableCount = SortableCount.fromInt 
                    (baseCopy.Length / (Degree.value degree))
            } |> Ok


    let createNr (degree:Degree) (baseArray:int[]) =
            {
                intSetsRollout.degree = degree; 
                baseArray = baseArray; 
                sortableCount = SortableCount.fromInt 
                    (baseArray.Length / (Degree.value degree))
            }

    let fromBitSet (degree:Degree) 
                   (baseArrays:bitSet seq) =
        result {
            let a = baseArrays |> Seq.map(fun a -> a.values)
                               |> Seq.collect(id)
                               |> Seq.toArray
            return! create degree a 
        }

    let fromIntArrays (degree:Degree) 
                      (baseArrays:int[] seq) =
        result {
            let a = baseArrays |> Seq.collect(id)
                               |> Seq.toArray
            return! create degree a 
        }

    let fromIntSets (degree:Degree) 
                    (intSets:intSet seq) =
        result {
            let a = intSets |> Seq.collect(fun ntS -> ntS.values)
                            |> Seq.toArray
            return! create degree a 
        }


    let toIntSet (intsRoll:intSetsRollout) =
        let d = (Degree.value intsRoll.degree)
        intsRoll.baseArray |> Seq.chunkBySize d
                           |> Seq.map(fun a -> {intSet.values = a})


    let copy (intsRoll:intSetsRollout) =
        let baseCopy = Array.zeroCreate intsRoll.baseArray.Length
        Array.Copy(intsRoll.baseArray, baseCopy, baseCopy.Length)
        {   intSetsRollout.degree=intsRoll.degree; 
            baseArray=baseCopy;
            sortableCount=intsRoll.sortableCount }


    let allBinary (degree:Degree) =
        let baseArray = BitSet.arrayOfAllFor degree
                        |> Array.collect(fun ia -> ia.values)
        create degree baseArray


    let isSorted (intsRoll:intSetsRollout) =
        intsRoll |> toIntSet 
                 |> Seq.forall(IntSet.isSorted)


    let sortedCount (intsRoll:intSetsRollout) =
        let d = (Degree.value intsRoll.degree)
        seq {0 .. d .. (intsRoll.baseArray.Length - 1)}
            |> Seq.filter (fun dex -> 
                    Combinatorics.isSortedOffsetI intsRoll.baseArray dex d)
            |> Seq.length


    let intSetHist (intsRollout:intSetsRollout) =
        intsRollout |> toIntSet
                    |> Seq.countBy id

    
    let removeDupes (intsRollout:intSetsRollout) =
        intsRollout |> toIntSet
                    |> Seq.distinct


    let uniqueUnsortedCount (intsRollout:intSetsRollout) =
        intsRollout |> removeDupes 
                    |> Seq.filter(fun bs -> not (IntSet.isSorted(bs)))
                    |> Seq.length



module BP64SetsRollout =

    let create (degree:Degree) 
               (baseArray:uint64[]) 
               (sortableCount:SortableCount) =

        if baseArray.Length < 0 + (Degree.value degree) then
           (sprintf "baseArray length %d is not a multiple of degree: %d:" 
             baseArray.Length (Degree.value degree)) 
             |> Error
        else
            let baseCopy = Array.zeroCreate baseArray.Length
            Array.Copy(baseArray, baseCopy, baseArray.Length)
            {
                bP64SetsRollout.degree = degree; 
                baseArray = baseCopy; 
                sortableCount = sortableCount
            } |> Ok


    let fromBitsP64 (degree:Degree)
                    (baseArrays:bitsP64 seq) =
        result {
            let a64 = baseArrays |> Seq.map(fun a -> a.values)
                                 |> Seq.collect(id)
                                 |> Seq.toArray
            return! create degree a64 (SortableCount.fromInt 
                                        ((64 * a64.Length) 
                                        / 
                                        (Degree.value degree)))
        }


    let toBitsP64 (bP64Roll:bP64SetsRollout) =
        let d = (Degree.value bP64Roll.degree)
        bP64Roll.baseArray |> Seq.chunkBySize d
                           |> Seq.map(fun a -> {bitsP64.values = a})


    let fromBitSet (degree:Degree)
                   (intBits:seq<bitSet>) =
        intBits |> BitsP64.fromBitSet
                |> fromBitsP64 degree


    let toBitSet (bP64Roll:bP64SetsRollout) =
        bP64Roll |> toBitsP64
                 |> BitsP64.toBitSets


    let fromIntSets (degree:Degree)
                    (intBits:seq<intSet>) =
        intBits |> BitsP64.fromIntSet
                |> fromBitsP64 degree

    let toIntSet (bP64Roll:bP64SetsRollout) =
        bP64Roll |> toBitsP64
                 |> BitsP64.toIntSets


    let isSorted (bp64Roll:bP64SetsRollout) =
        bp64Roll |> toBitSet 
                 |> Seq.forall(BitSet.isSorted)


    let bitSetHist (bp64Roll:bP64SetsRollout) =
        bp64Roll |> toBitSet
                 |> Seq.countBy id


    let copy (bP64Roll:bP64SetsRollout) =
        let baseCopy = Array.zeroCreate bP64Roll.baseArray.Length
        Array.Copy(bP64Roll.baseArray, baseCopy, baseCopy.Length)
        {   bP64SetsRollout.degree=bP64Roll.degree; 
            baseArray=baseCopy;
            sortableCount=bP64Roll.sortableCount }


    let allBinary (degree:Degree) =
        let arraySets = BitsP64.arrayOfAllFor degree
        let baseArray = arraySets
                        |> Array.collect(fun ia -> ia.values)
        create degree baseArray (degree |> Degree.binExp |> SortableCount.fromInt)


    let removeDupes (bP64Roll:bP64SetsRollout) =
        let records = Record64Array.make (bP64Roll.degree)
        bP64Roll |> toBitSet
                 |> Seq.map(BitSet.toUint64)
                 |> Seq.iter(Record64Array.recordPosition records)
        records |> Record64Array.toBitSets bP64Roll.degree


    let uniqueUnsortedCount (bP64Roll:bP64SetsRollout) =
        bP64Roll |> removeDupes 
                 |> Seq.filter(fun bs -> not (BitSet.isSorted(bs)))
                 |> Seq.length



module SortableSetRollout =

    let getDegree (sortableSetRollout:sortableSetRollout) =
        match sortableSetRollout with
        | sortableSetRollout.Int isr -> isr.degree
        | sortableSetRollout.Bp64 bp64r -> bp64r.degree

    let copy (sortableRollout:sortableSetRollout) =
        match sortableRollout with
        | Int  isr ->    isr |> IntSetsRollout.copy 
                             |> sortableSetRollout.Int
        | Bp64  bp64r -> bp64r |> BP64SetsRollout.copy 
                               |> sortableSetRollout.Bp64


    let isSorted (sortableRollout:sortableSetRollout) =
        match sortableRollout with
        | Int  isr ->    isr |> IntSetsRollout.isSorted
        | Bp64  bp64r -> bp64r |> BP64SetsRollout.isSorted


    let uniqueUnsortedCount (sortableRollout:sortableSetRollout) =
        match sortableRollout with
        | Int  isr ->    isr |> IntSetsRollout.uniqueUnsortedCount
                             |> SortableCount.fromInt
        | Bp64  bp64r -> bp64r |> BP64SetsRollout.uniqueUnsortedCount 
                               |> SortableCount.fromInt


    let toIntSet (sortableRollout:sortableSetRollout) =
        match sortableRollout with
        | Int  isr ->    isr |> IntSetsRollout.toIntSet
        | Bp64  bp64r -> bp64r |> BP64SetsRollout.toBitSet 
                               |> Seq.map(BitSet.toIntSet)


    //let intBitsHist (sortableRollout:sortableSetRollout) =
    //    match sortableRollout with
    //    | Int  isr ->    isr |> IntSetsRollout.intSetHist
    //    | Bp64  bp64r -> bp64r |> BP64SetsRollout.bitSetHist


    let removeDupes (ssR:sortableSetRollout) =
        let degree = ssR |> getDegree
        match ssR with
        | Int  isr ->   
            result {
               let! irDedup = isr |> IntSetsRollout.removeDupes
                                  |> Seq.map(fun ia->ia.values)
                                  |> IntSetsRollout.fromIntArrays degree
               return sortableSetRollout.Int irDedup
            }
        | Bp64  bp64r -> 
            result {
               let! brDedup = bp64r |> BP64SetsRollout.removeDupes
                                    |> BP64SetsRollout.fromBitSet degree
               return sortableSetRollout.Bp64 brDedup
            }