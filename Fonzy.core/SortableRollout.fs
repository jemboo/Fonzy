namespace global
open System

type IntSetsRollout = 
        {   degree:Degree; 
            baseArray:int[]; 
            sortableCount:SortableCount  }


type bP32SetsRollout = 
        {   degree:Degree; 
            baseArray:uint[]; 
            sortableCount:SortableCount  }


type bP64SetsRollout = 
        {   degree:Degree; 
            baseArray:uint64[]; 
            sortableCount:SortableCount  }


type SortableRollout =
    | Int of IntSetsRollout
    | Bp32 of bP32SetsRollout
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

    //let isSorted (ssRollout:IntSetsRollout) =
    //    let d = (Degree.value ssRollout.degree)
    //    seq {0 .. d .. (ssRollout.baseArray.Length - 1)}
    //        |> Seq.forall(fun dex -> 
    //                Combinatorics.isSortedOffset ssRollout.baseArray dex d)
        

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



module BP32SetsRollout =

    let create (degree:Degree) 
               (baseArray:uint[] ) 
               (sortableCount:SortableCount) =
        if baseArray.Length < 0 + (Degree.value degree) then
            Error (sprintf "baseArray length %d is not a multiple of degree: %d:" 
                    baseArray.Length (Degree.value degree))
        else
            let baseCopy = Array.zeroCreate baseArray.Length
            Array.Copy(baseArray, baseCopy, baseArray.Length)
            {
                bP32SetsRollout.degree = degree; 
                baseArray = baseCopy; 
                sortableCount = sortableCount
            } |> Ok


    let fromBitsP32 (degree:Degree) 
                    (baseArrays:bitsP32 seq) =
        result {
            let a = baseArrays |> Seq.map(fun a -> a.values)
                               |> Seq.collect(id)
                               |> Seq.toArray
            return! create degree a (SortableCount.fromInt a.Length)
        }


    let toBitsP32 (bp32Roll:bP32SetsRollout) =
        let d = (Degree.value bp32Roll.degree)
        bp32Roll.baseArray |> Seq.chunkBySize d
                           |> Seq.map(fun a -> {bitsP32.values = a})


    let toIntBits (bp32Roll:bP32SetsRollout) =
        bp32Roll |> toBitsP32
                 |> BitsP32.toIntBits


    let isSorted (bp32Roll:bP32SetsRollout) =
        bp32Roll |> toIntBits 
                 |> Seq.forall(IntBits.isSorted)


    let intBitsHist (bp32Roll:bP32SetsRollout) =
        bp32Roll |> toIntBits
                 |> Seq.countBy id
                 |> Seq.toArray


    let copy (src:bP32SetsRollout) =
        let baseCopy = Array.zeroCreate src.baseArray.Length
        Array.Copy(src.baseArray, baseCopy, baseCopy.Length)
        {   bP32SetsRollout.degree = src.degree; 
            baseArray = baseCopy;
            sortableCount = src.sortableCount }


    let allBinary (degree:Degree) =
        let arraySets = BitsP32.arrayOfAllFor degree
        let baseArray = arraySets
                        |> Array.collect(fun ia -> ia.values)
        create degree baseArray (SortableCount.fromInt arraySets.Length)



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
            let a = baseArrays |> Seq.map(fun a -> a.values)
                               |> Seq.collect(id)
                               |> Seq.toArray
            return! create degree a (SortableCount.fromInt a.Length)
        }


    let toBitsP64 (ssRollout:bP64SetsRollout) =
        let d = (Degree.value ssRollout.degree)
        ssRollout.baseArray |> Seq.chunkBySize d
                            |> Seq.map(fun a -> {bitsP64.values = a})


    let toIntBits (ssRollout:bP64SetsRollout) =
        ssRollout |> toBitsP64
                  |> BitsP64.toIntBits


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
        create degree baseArray (SortableCount.fromInt arraySets.Length)



module SortableRollout =

    let copy (sortableRollout:SortableRollout) =
        match sortableRollout with
        | Int  isr ->    isr |> IntSetsRollout.copy |> SortableRollout.Int
        | Bp32  bp32r -> bp32r |> BP32SetsRollout.copy |> SortableRollout.Bp32
        | Bp64  bp64r -> bp64r |> BP64SetsRollout.copy |> SortableRollout.Bp64


    let isSorted (sortableRollout:SortableRollout) =
        match sortableRollout with
        | Int  isr ->    isr |> IntSetsRollout.isSorted
        | Bp32  bp32r -> bp32r |> BP32SetsRollout.isSorted
        | Bp64  bp64r -> bp64r |> BP64SetsRollout.isSorted


    let intBitsHist (sortableRollout:SortableRollout) =
        match sortableRollout with
        | Int  isr ->    isr |> IntSetsRollout.intBitsHist
        | Bp32  bp32r -> bp32r |> BP32SetsRollout.intBitsHist
        | Bp64  bp64r -> bp64r |> BP64SetsRollout.intBitsHist