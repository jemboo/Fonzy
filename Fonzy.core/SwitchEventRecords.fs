namespace global
open System

type SwitchUses = {switchCount:SwitchCount; weights:int[]}
module SwitchUses =
   let createEmpty (switchCount:SwitchCount) =
       {switchCount=switchCount; 
        weights=Array.init (SwitchCount.value switchCount) (fun i -> 0)}

   let init (weights:int[]) =
           {
                switchCount = (SwitchCount.fromInt weights.Length); 
                weights=weights
           }

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


   let lastUsedIndexes (switchCount:SwitchCount) 
                       (stseq:seq<SwitchUses>) =            
       let stRet = createEmpty switchCount
       let wgts = getWeights stRet
       let Recordo (stRec:int[]) (stData:SwitchUses) =
           let lui = lastUsedIndex stData
           stRec.[lui]<-stRec.[lui] + 1
       stseq |> Seq.iter(fun st -> Recordo wgts st)
       stRet


   let usedSwitchCount (switchUses:SwitchUses) = 
       getWeights switchUses |> Array.filter(fun i-> i > 0) 
                             |> Array.length
                             |> SwitchCount.fromInt


   let getSwitchActionTotal (switchUses:SwitchUses) =
       (getWeights switchUses) |> Array.sum


   let entropyBits (switchUses:SwitchUses) =
       (getWeights switchUses) |> Combinatorics.entropyBits


   let getRefinedStageCount (switchUses:SwitchUses) 
                            (sorter:Sorter) =
       result {
           let! usedSwitches = getUsedSwitches switchUses sorter
           let degree = sorter.degree
           return! Stage.getStageCount degree usedSwitches
       }


   let getRefinedSorter (switchUses:SwitchUses) 
                        (sorter:Sorter) =
       result {
           let! usedSwitches = getUsedSwitches switchUses sorter
           let degree = sorter.degree
           let stages = Stage.fromSwitches degree usedSwitches |> Seq.toArray
           let switches = seq {for i in 0 .. (stages.Length - 1) do yield! stages.[i].switches}
           return Sorter.fromSwitches degree switches
       }


   let getSwitchAndStageUses (sorter:Sorter) 
                             (switchUses:SwitchUses) =
       result
           {
               let! refinedStageCount = (getRefinedStageCount switchUses sorter)
               let switchUseCount = (usedSwitchCount switchUses)
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



type SwitchUseB64 = {switchCount:SwitchCount; weights:uint64[]}
module SwitchUseB64 =
   let createEmpty (switchCount:SwitchCount) =
       {switchCount=switchCount; 
        weights=Array.init (SwitchCount.value switchCount) (fun i -> 0UL)}

   let init (switchCount:SwitchCount) (weights:int[]) =
       let zz = createEmpty switchCount
       ByteUtils.stripeWrite zz.weights weights 0
       zz

   let getWeights switchUses = switchUses.weights
   let switchCount switchUses = (SwitchCount.value switchUses.switchCount)

   let toSwitchUses (switchUseB64:SwitchUseB64) = 
       let suA = switchUseB64.weights
                 |> Array.map(ByteUtils.trueBitCount64)
       SwitchUses.init suA



type SortableUses = {sortableCount:SortableCount; weights:int[]}
module SortableUses =
    let createEmpty (sortableCount:SortableCount) =
        {
            sortableCount=sortableCount; 
            weights=Array.init (SortableCount.value sortableCount) (fun i -> 0)
        }
    let getWeights sortableUses = sortableUses.weights
    let sortableCount sortableUses = (SortableCount.value sortableUses.sortableCount)
