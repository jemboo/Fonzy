﻿namespace global
open System

module SwitchUses =
   let createNone = {switchUses.weights=[||]}

   let createEmpty (switchCount:SwitchCount) =
       {weights=Array.zeroCreate (SwitchCount.value switchCount)}

   let createOnes (switchCount:SwitchCount) =
        {
            weights=Array.init (SwitchCount.value switchCount) 
                               (fun _ -> 1)
        }

   let init (weights:int[]) =  { weights=weights}
   let getWeights switchUses = switchUses.weights
   let switchCount switchUses = switchUses.weights.Length

   let add (trackerA:switchUses) 
           (trackerB:switchUses) =
       if ((switchCount trackerA) <> (switchCount trackerB))  then
           (sprintf "switchCounts: %d, %d are not equal" 
                   (switchCount trackerA) (switchCount trackerB)) |> Error
       else
           let weightsSum = Array.map2 (+) (getWeights trackerA) (getWeights trackerB) 
           {
               weights = weightsSum;
           } |> Ok

           
   let append (sfx:switchUses) 
              (pfx:switchUses) =
        {switchUses.weights = sfx.weights |> Array.append pfx.weights}


   let getUsedSwitches (switchUses:switchUses) 
                       (sorter:sorter) =
       let switches = sorter.switches
       let weights = (getWeights switchUses)
       weights |> Seq.mapi(fun i w -> i,w)
       |> Seq.filter(fun t -> (snd t) > 0 )
       |> Seq.map(fun t -> switches.[(fst t)])
       |> Seq.toArray


   let lastUsedIndex (st:switchUses) =
       let w = (getWeights st)
       w
           |> Seq.mapi(fun i x -> (i, x))
           |> Seq.filter(fun tup -> (snd tup) > 0)
           |> Seq.maxBy(fst) |> fst


   let lastUsedIndexes (switchCount:SwitchCount) 
                       (stseq:seq<switchUses>) =            
       let stRet = createEmpty switchCount
       let wgts = getWeights stRet
       let Recordo (stRec:int[]) (stData:switchUses) =
           let lui = lastUsedIndex stData
           stRec.[lui]<-stRec.[lui] + 1
       stseq |> Seq.iter(fun st -> Recordo wgts st)
       stRet


   let usedSwitchCount (switchUses:switchUses) = 
       getWeights switchUses |> Array.filter(fun i-> i > 0) 
                             |> Array.length
                             |> SwitchCount.fromInt


   let getSwitchActionTotal (switchUses:switchUses) =
       (getWeights switchUses) |> Array.sum


   let entropyBits (switchUses:switchUses) =
       (getWeights switchUses) |> Combinatorics.entropyBits


   let getRefinedStageCount (switchUses:switchUses) 
                            (sorter:sorter) =
       result {
           let usedSwitches = getUsedSwitches switchUses sorter
           let degree = sorter.degree
           return! Stage.getStageCount degree usedSwitches
       }


   let getRefinedSorter (switchUses:switchUses) 
                        (sorter:sorter) =
       result {
           let usedSwitches = getUsedSwitches switchUses sorter
           let degree = sorter.degree
           let stages = Stage.fromSwitches degree usedSwitches |> Seq.toArray
           let switches = seq {for i in 0 .. (stages.Length - 1) do yield! stages.[i].switches}
           return Sorter.fromSwitches degree switches
       }


   let getSwitchAndStageUses (sorter:sorter) 
                             (switchUses:switchUses) =
       result
           {
               let refinedStageCount = (getRefinedStageCount switchUses sorter)
               let switchUseCount = (usedSwitchCount switchUses)
               return switchUseCount, refinedStageCount
           }


   let reportResultStats stats =
       StringUtils.printSeqfToRow 
           (fun res ->
           match res with
           | Ok (s,a,b,c,d) -> sprintf "%f %d %d %d" 
                                           a b 
                                           (SwitchCount.value c) 
                                           (StageCount.value d)
           | Error msg -> sprintf "%s" msg ) 
           stats


   let reportStats stats =
       StringUtils.printSeqfToRow 
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
            weights=Array.zeroCreate (SortableCount.value sortableCount)
        }
    let getWeights sortableUses = sortableUses.weights
    let sortableCount sortableUses = (SortableCount.value sortableUses.sortableCount)
