namespace global
open System

type IntSetsRollout = 
        {   degree:Degree; 
            baseArray:int[]; 
            sortableCount:SortableCount  }



type bP64SetsRollout = 
        {   degree:Degree; 
            baseArray:uint64[]; 
            sortableCount:SortableCount  }


type sortableSetRollout =
    | Int of IntSetsRollout
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
                IntSetsRollout.degree = degree; 
                baseArray = baseCopy; 
                sortableCount = SortableCount.fromInt 
                    (baseCopy.Length / (Degree.value degree))
            } |> Ok


    let fromIntBits (degree:Degree) 
                    (baseArrays:IntBits seq) =
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


    let toIntBits (intsRoll:IntSetsRollout) =
        let d = (Degree.value intsRoll.degree)
        intsRoll.baseArray |> Array.chunkBySize d
                            |> Array.map(fun a -> {IntBits.values = a})


    let copy (intsRoll:IntSetsRollout) =
        let baseCopy = Array.zeroCreate intsRoll.baseArray.Length
        Array.Copy(intsRoll.baseArray, baseCopy, baseCopy.Length)
        {   IntSetsRollout.degree=intsRoll.degree; 
            baseArray=baseCopy;
            sortableCount=intsRoll.sortableCount }


    let allBinary (degree:Degree) =
        let baseArray = IntBits.arrayOfAllFor degree
                        |> Array.collect(fun ia -> ia.values)
        create degree baseArray


    let isSorted (intsRoll:IntSetsRollout) =
        intsRoll |> toIntBits 
                 |> Array.forall(IntBits.isSorted)


    let sortedCount (intsRoll:IntSetsRollout) =
        let d = (Degree.value intsRoll.degree)
        seq {0 .. d .. (intsRoll.baseArray.Length - 1)}
            |> Seq.filter (fun dex -> 
                    Combinatorics.isSortedOffset intsRoll.baseArray dex d)
            |> Seq.length


    let intBitsDistinct (intsRoll:IntSetsRollout) =
        intsRoll |> toIntBits
                 |> Seq.distinct
                 |> Seq.toArray


    let intBitsHist (intsRollout:IntSetsRollout) =
        intsRollout |> toIntBits
                    |> Seq.countBy id
                    |> Seq.toArray

    
    let removeDupes (intsRollout:IntSetsRollout) =
        let records = Record64Array.make (intsRollout.degree)
        intsRollout |> toIntBits
                    |> Seq.map(IntBits.toUint64)
                    |> Seq.iter(fun yak -> Record64Array.recordPosition records yak)
        records |> Record64Array.toIntArrays intsRollout.degree



module BP64SetsRollout =

    let create (degree:Degree) 
               (baseArray:uint64[]) 
               (sortableCount:SortableCount) =

        if baseArray.Length < 0 + (Degree.value degree) then
            Error (sprintf "baseArray length %d is not a multiple of degree: %d:" 
                    baseArray.Length (Degree.value degree))
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
                                        ((64 * a64.Length) / (Degree.value degree)))
        }


    let toBitsP64 (ssRollout:bP64SetsRollout) =
        let d = (Degree.value ssRollout.degree)
        ssRollout.baseArray |> Seq.chunkBySize d
                            |> Seq.map(fun a -> {bitsP64.values = a})


    let fromIntBits (degree:Degree)
                    (intBits:seq<IntBits>) =
        intBits |> BitsP64.fromIntBits
                |> fromBitsP64 degree


    let toIntBits (ssRollout:bP64SetsRollout) =
        ssRollout |> toBitsP64
                  |> BitsP64.toIntBits
                  |> Seq.toArray

    let isSorted (bp64Roll:bP64SetsRollout) =
        bp64Roll |> toIntBits 
                 |> Seq.forall(IntBits.isSorted)


    let intBitsHist (bp64Roll:bP64SetsRollout) =
        bp64Roll |> toIntBits
                 |> Seq.countBy id
                 |> Seq.toArray


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
        bP64Roll |> toIntBits
                 |> Seq.map(IntBits.toUint64)
                 |> Seq.iter(fun yak -> Record64Array.recordPosition records yak)
        records |> Record64Array.toIntArrays bP64Roll.degree



module SortableSetRollout =

    let copy (sortableRollout:sortableSetRollout) =
        match sortableRollout with
        | Int  isr ->    isr |> IntSetsRollout.copy |> sortableSetRollout.Int
        | Bp64  bp64r -> bp64r |> BP64SetsRollout.copy |> sortableSetRollout.Bp64


    let isSorted (sortableRollout:sortableSetRollout) =
        match sortableRollout with
        | Int  isr ->    isr |> IntSetsRollout.isSorted
        | Bp64  bp64r -> bp64r |> BP64SetsRollout.isSorted


    let toIntBits (sortableRollout:sortableSetRollout) =
        match sortableRollout with
        | Int  isr ->    isr |> IntSetsRollout.toIntBits
        | Bp64  bp64r -> bp64r |> BP64SetsRollout.toIntBits


    let intBitsHist (sortableRollout:sortableSetRollout) =
        match sortableRollout with
        | Int  isr ->    isr |> IntSetsRollout.intBitsHist
        | Bp64  bp64r -> bp64r |> BP64SetsRollout.intBitsHist


    let removeDupes (sortableRollout:sortableSetRollout) =
        match sortableRollout with
        | Int  isr ->    isr |> IntSetsRollout.removeDupes
        | Bp64  bp64r -> bp64r |> BP64SetsRollout.removeDupes


    let sorterCompres (sortableRollout:sortableSetRollout) =
        None