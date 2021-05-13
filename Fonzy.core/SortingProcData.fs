namespace global
open System

type SwitchUses = {switchCount:SwitchCount; weights:int[]}
module SwitchUses =
   let createEmpty (switchCount:SwitchCount) =
       {switchCount=switchCount; 
        weights=Array.init (SwitchCount.value switchCount) (fun i -> 0)}

   let create (switchCount:SwitchCount) (weights:int[]) =
       if  (SwitchCount.value switchCount) = weights.Length then
           {switchCount=switchCount; weights=weights} |> Ok
       else Error (sprintf "switchCount: %d is not equal to weights length: %d" 
                            (SwitchCount.value switchCount) weights.Length) 

   let getWeights switchUses = switchUses.weights
   let switchCount switchUses = (SwitchCount.value switchUses.switchCount)

   let Add (trackerA:SwitchUses) (trackerB:SwitchUses) =
       if ((switchCount trackerA) <> (switchCount trackerB))  then
           (sprintf "switchCounts: %d, %d are not equal" 
                   (switchCount trackerA) (switchCount trackerB)) |> Error
       else
           let weightsSum = Array.map2 (+) (getWeights trackerA) (getWeights trackerB) 
           {
               switchCount = SwitchCount.fromInt weightsSum.Length
               weights = weightsSum;
           } |> Ok

   let getUsedSwitches (switchUses:SwitchUses) (sorter:Sorter) =
       let useCount = SwitchCount.value switchUses.switchCount
       let switches = sorter.switches
       let weights = (getWeights switchUses)
       if (switches.Length <> useCount) then
           sprintf "useCount=%d, SwitchCount=%d" useCount switches.Length |> Error
       else
           let res = weights |> Seq.mapi(fun i w -> i,w)
                             |> Seq.filter(fun t -> (snd t) > 0 )
                             |> Seq.map(fun t -> switches.[(fst t)])
                             |> Seq.toArray
           res |> Ok

   let lastUsedIndex (st:SwitchUses) =
       let w = (getWeights st)
       w
           |> Seq.mapi(fun i x -> (i, x))
           |> Seq.filter(fun tup -> (snd tup) > 0)
           |> Seq.maxBy(fst) |> fst

   let lastUsedIndexes (switchCount:SwitchCount) (stseq:seq<SwitchUses>) =            
       let stRet = createEmpty switchCount
       let wgts = getWeights stRet
       let Recordo (stRec:int[]) (stData:SwitchUses) =
           let lui = lastUsedIndex stData
           stRec.[lui]<-stRec.[lui] + 1
       stseq |> Seq.iter(fun st -> Recordo wgts st)
       stRet

   let toUsedSwitchCount (switchUses:SwitchUses) = 
       getWeights switchUses |> Array.filter(fun i->i>0) 
                             |> Array.length
                             |> SwitchCount.create ""

   let getSwitchActionTotal (switchUses:SwitchUses) =
       (getWeights switchUses) |> Array.sum

   let entropyBits (switchUses:SwitchUses) =
       (getWeights switchUses) |> Combinatorics.entropyBits

   let getRefinedStageCount (switchUses:SwitchUses) (sorter:Sorter) =
       result {
           let! usedSwitches = getUsedSwitches switchUses sorter
           let degree = sorter.degree
           return! Stage.getStageCount degree usedSwitches
       }

   let getRefinedSorter (switchUses:SwitchUses) (sorter:Sorter) =
       result {
           let! usedSwitches = getUsedSwitches switchUses sorter
           let degree = sorter.degree
           let stages = Stage.mergeSwitchesIntoStages degree usedSwitches |> Seq.toArray
           let switches = seq {for i in 0 .. (stages.Length - 1) do yield! stages.[i].switches}
           return Sorter.create degree switches
       }

   let getSwitchAndStageUses (sorter:Sorter) (switchUses:SwitchUses) =
       result
           {
               let! refinedStageCount = (getRefinedStageCount switchUses sorter)
               let! switchUseCount = (toUsedSwitchCount switchUses)
               return switchUseCount, refinedStageCount
           }

   let reportResultStats stats =
       StringUtils.printArrayf 
           (fun res ->
           match res with
           | Ok (s,a,b,c,d) -> sprintf "%f %d %d %d" 
                                           a b 
                                           (SwitchCount.value c) 
                                           (StageCount.value d)
           | Error msg -> sprintf "%s" msg ) 
           stats

   let reportStats stats =
       StringUtils.printArrayf 
           (fun (s,a,b,c,d) -> sprintf "%f %d %d %d" 
                                           a b 
                                           (SwitchCount.value c) 
                                           (StageCount.value d))
           stats

type SortableUses = {sortableCount:SortableCount; weights:int[]}
module SortableUses =
    let createEmpty (sortableCount:SortableCount) =
        {
            sortableCount=sortableCount; 
            weights=Array.init (SortableCount.value sortableCount) (fun i -> 0)
        }
    let getWeights sortableUses = sortableUses.weights
    let sortableCount sortableUses = (SortableCount.value sortableUses.sortableCount)

type SwitchEventRollout = {
            switchCount:SwitchCount; 
            sortableCount:SortableCount; 
            useRoll:int[]}

module SwitchEventRollout =
    let create (switchCount:SwitchCount) (sortableCount:SortableCount) = 
        {
            switchCount=switchCount;
            sortableCount=sortableCount;
            useRoll = Array.zeroCreate ((SwitchCount.value switchCount) * 
                            (SortableCount.value sortableCount))
        }

    let toSwitchUses (switchUseRollout:SwitchEventRollout) =
        let swCt = (SwitchCount.value switchUseRollout.switchCount)
        let useWeights = Array.zeroCreate swCt
        let upDateSwU dex v =
            let swUdex = dex % swCt
            useWeights.[swUdex] <- useWeights.[swUdex] + v

        switchUseRollout.useRoll |> Array.iteri(fun dex v -> upDateSwU dex v)

        {
            SwitchUses.switchCount = switchUseRollout.switchCount;
            SwitchUses.weights = useWeights
        }

type SortableSetRollout = {
            degree:Degree; 
            baseArray:int[]; 
            sortableCount:SortableCount
        }

module SortableSetRollout =
    let create (degree:Degree) (baseArray:int[] ) =
        if baseArray.Length < 0 + (Degree.value degree) then
            Error (sprintf "baseArray length %d is not a multiple of degree: %d:" 
                    baseArray.Length (Degree.value degree))
        else
            let baseCopy = Array.zeroCreate baseArray.Length
            Array.Copy(baseArray, baseCopy, baseArray.Length)
            {
                SortableSetRollout.degree=degree; 
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

    let toSortableIntArrays (ssRollout:SortableSetRollout) =
        let d = (Degree.value ssRollout.degree)
        ssRollout.baseArray |> Array.chunkBySize d
                            |> Array.map(fun a -> {IntBits.values = a})

    let copy (sortableSetRollout:SortableSetRollout) =
        let baseCopy = Array.zeroCreate sortableSetRollout.baseArray.Length
        Array.Copy(sortableSetRollout.baseArray, baseCopy, baseCopy.Length)
        {
            SortableSetRollout.degree=sortableSetRollout.degree; 
            baseArray=baseCopy;
            sortableCount=sortableSetRollout.sortableCount
        }

    let allBinary (degree:Degree) =
        let baseArray = IntBits.allBinaryArray degree
                        |> Array.collect(fun ia -> ia.values)
        create degree baseArray

    let isSorted (ssRollout:SortableSetRollout) =
        let d = (Degree.value ssRollout.degree)
        seq {0 .. d .. (ssRollout.baseArray.Length - 1)}
            |> Seq.forall(fun dex -> 
                    Combinatorics.isSortedOffset ssRollout.baseArray dex d)
        
    let sortedCount (ssRollout:SortableSetRollout) =
        let d = (Degree.value ssRollout.degree)
        seq {0 .. d .. (ssRollout.baseArray.Length - 1)}
            |> Seq.filter (fun dex -> 
                    Combinatorics.isSortedOffset ssRollout.baseArray dex d)
            |> Seq.length

    let distinctSortableSets (ssRollout:SortableSetRollout) =
        ssRollout |> toSortableIntArrays
                  |> Seq.distinct
                  |> Seq.toArray

    let histogramOfSortedSortables (ssRollout:SortableSetRollout) =
        ssRollout |> toSortableIntArrays
                  |> Seq.countBy id
                  |> Seq.toArray