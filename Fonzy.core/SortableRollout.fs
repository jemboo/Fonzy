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
    | Int of bP32SetsRollout
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


    let fromSortableIntArrays (degree:Degree) 
                              (baseArrays:IntBits seq) =
        result {
            let a = baseArrays |> Seq.map(fun a -> a.values)
                               |> Seq.collect(id)
                               |> Seq.toArray
            return! create degree a 
        }


    let toSortableIntArrays (ssRollout:IntSetsRollout) =
        let d = (Degree.value ssRollout.degree)
        ssRollout.baseArray |> Array.chunkBySize d
                            |> Array.map(fun a -> {IntBits.values = a})


    let copy (sortableSetRollout:IntSetsRollout) =
        let baseCopy = Array.zeroCreate sortableSetRollout.baseArray.Length
        Array.Copy(sortableSetRollout.baseArray, baseCopy, baseCopy.Length)
        {   IntSetsRollout.degree=sortableSetRollout.degree; 
            baseArray=baseCopy;
            sortableCount=sortableSetRollout.sortableCount }


    let allBinary (degree:Degree) =
        let baseArray = IntBits.arrayOfAllFor degree
                        |> Array.collect(fun ia -> ia.values)
        create degree baseArray


    let isSorted (ssRollout:IntSetsRollout) =
        let d = (Degree.value ssRollout.degree)
        seq {0 .. d .. (ssRollout.baseArray.Length - 1)}
            |> Seq.forall(fun dex -> 
                    Combinatorics.isSortedOffset ssRollout.baseArray dex d)
        

    let sortedCount (ssRollout:IntSetsRollout) =
        let d = (Degree.value ssRollout.degree)
        seq {0 .. d .. (ssRollout.baseArray.Length - 1)}
            |> Seq.filter (fun dex -> 
                    Combinatorics.isSortedOffset ssRollout.baseArray dex d)
            |> Seq.length


    let distinctSortableSets (ssRollout:IntSetsRollout) =
        ssRollout |> toSortableIntArrays
                  |> Seq.distinct
                  |> Seq.toArray


    let histogramOfSortedSortables (ssRollout:IntSetsRollout) =
        ssRollout |> toSortableIntArrays
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


    let fromSortableIntArrays (degree:Degree) 
                              (baseArrays:bitsP32 seq) 
                              (sortableCount:SortableCount) =
        result {
            let a = baseArrays |> Seq.map(fun a -> a.values)
                               |> Seq.collect(id)
                               |> Seq.toArray
            return! create degree a sortableCount
        }


    let toBitsP32Arrays (ssRollout:bP32SetsRollout) =
        let d = (Degree.value ssRollout.degree)
        ssRollout.baseArray |> Array.chunkBySize d
                            |> Array.map(fun a -> {bitsP32.values = a})


    let copy (src:bP32SetsRollout) =
        let baseCopy = Array.zeroCreate src.baseArray.Length
        Array.Copy(src.baseArray, baseCopy, baseCopy.Length)
        {   bP32SetsRollout.degree=src.degree; 
            baseArray=baseCopy;
            sortableCount=src.sortableCount }

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


    let fromSortableIntArrays (degree:Degree) 
                              (baseArrays:bitsP64 seq) 
                              (sortableCount:SortableCount) =
        result {
            let a = baseArrays |> Seq.map(fun a -> a.values)
                               |> Seq.collect(id)
                               |> Seq.toArray
            return! create degree a sortableCount
        }


    let toBitsP32Arrays (ssRollout:bP64SetsRollout) =
        let d = (Degree.value ssRollout.degree)
        ssRollout.baseArray |> Array.chunkBySize d
                            |> Array.map(fun a -> {bitsP64.values = a})


    let copy (sortableSetRollout:bP64SetsRollout) =
        let baseCopy = Array.zeroCreate sortableSetRollout.baseArray.Length
        Array.Copy(sortableSetRollout.baseArray, baseCopy, baseCopy.Length)
        {   bP64SetsRollout.degree=sortableSetRollout.degree; 
            baseArray=baseCopy;
            sortableCount=sortableSetRollout.sortableCount }


    let allBinary (degree:Degree) =
        let arraySets = BitsP64.arrayOfAllFor degree
        let baseArray = arraySets
                        |> Array.collect(fun ia -> ia.values)
        create degree baseArray (SortableCount.fromInt arraySets.Length)