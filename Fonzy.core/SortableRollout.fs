namespace global
open System

type SwitchEventRollout = {
            switchCount:SwitchCount; 
            sortableCount:SortableCount; 
            useRoll:int[]}

module SwitchEventRollout =
    let create (switchCount:SwitchCount) 
               (sortableCount:SortableCount) = 
        {   switchCount=switchCount;
            sortableCount=sortableCount;
            useRoll = Array.zeroCreate 
                        ((SwitchCount.value switchCount) * 
                        (SortableCount.value sortableCount))    }


    let toSwitchUses (switchUseRollout:SwitchEventRollout) =
        let swCt = (SwitchCount.value switchUseRollout.switchCount)
        let useWeights = Array.zeroCreate swCt
        let upDateSwU dex v =
            let swUdex = dex % swCt
            useWeights.[swUdex] <- useWeights.[swUdex] + v

        switchUseRollout.useRoll |> Array.iteri(fun dex v -> upDateSwU dex v)

        {   SwitchUses.switchCount = switchUseRollout.switchCount;
            SwitchUses.weights = useWeights    }


type IntSetsRollout = 
        {   degree:Degree; 
            baseArray:int[]; 
            sortableCount:SortableCount  }


module IntSetsRollout =
    let create (degree:Degree) (baseArray:int[] ) =
        if baseArray.Length < 0 + (Degree.value degree) then
            Error (sprintf "baseArray length %d is not a multiple of degree: %d:" 
                    baseArray.Length (Degree.value degree))
        else
            let baseCopy = Array.zeroCreate baseArray.Length
            Array.Copy(baseArray, baseCopy, baseArray.Length)
            {
                IntSetsRollout.degree=degree; 
                baseArray=baseCopy; 
                sortableCount= SortableCount.fromInt 
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