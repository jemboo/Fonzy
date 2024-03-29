﻿namespace global
open System

type switchEventRolloutInt = {
            switchCount:SwitchCount; 
            sortableCount:SortableCount; 
            useRoll:bitSet}


type switchEventRolloutBp64 = {
        switchCount:SwitchCount;
        sortableCount:SortableCount;
        sortableBlockCount:int;
        useRoll:bitsP64 }


type switchEventRollout =
     | Int of switchEventRolloutInt
     | Bp64 of switchEventRolloutBp64


module SwitchEventRolloutInt =
    let create (switchCount:SwitchCount) 
               (sortableCount:SortableCount) = 
        {   
            switchEventRolloutInt.switchCount = switchCount;
            sortableCount = sortableCount;
            useRoll = BitSet.zeroCreate 
                        ((SwitchCount.value switchCount) * 
                        (SortableCount.value sortableCount))    
        }

    let init (weights:int[])
             (sortableCount:SortableCount) = 
        let zz = create (SwitchCount.fromInt weights.Length) sortableCount
        weights |> Array.iteri(fun i _ -> zz.useRoll.values.[i] <- weights.[i])
        zz

    let toSwitchUses (switchEvents:switchEventRolloutInt) =
        let swCt = (SwitchCount.value switchEvents.switchCount)
        let useWeights = Array.zeroCreate swCt
        let upDateSwU dex v =
            let swUdex = dex % swCt
            useWeights.[swUdex] <- useWeights.[swUdex] + v

        switchEvents.useRoll.values |> Array.iteri(fun dex v -> upDateSwU dex v)
        { switchUses.weights = useWeights }


module SwitchEventRolloutBp64 =
    let create (switchCount:SwitchCount) 
               (sortableCount:SortableCount) = 

        let blockCount = (SortableCount.value sortableCount) |> BitsP64.pBlocksFor
        let ur = BitsP64.zeroCreate
                              ((SwitchCount.value switchCount) * 
                               blockCount)

        {   switchCount = switchCount;
            sortableCount = sortableCount;
            sortableBlockCount = blockCount;
            useRoll = ur 
        }

    let init (sortableCount:SortableCount)
             (weights:int[]) =
        let zz = create (SwitchCount.fromInt weights.Length) 
                        sortableCount
        weights |> Array.iteri(fun i _ -> 
                    zz.useRoll.values.[i] <- weights.[i] |> uint64)
        zz


    let toSwitchUses (switchEvents:switchEventRolloutBp64) =
        let switchCt = (SwitchCount.value switchEvents.switchCount)
        let weights = switchEvents.useRoll.values
                       |> Array.map(fun l -> ByteUtils.trueBitCount64 l )
                       |> CollectionUtils.wrapAndSumCols switchCt

        { switchUses.weights = weights }
            

module SwitchEventRollout =
    let toSwitchUses (switchEventRollout:switchEventRollout) =
        match switchEventRollout with
        | Int si -> si |> SwitchEventRolloutInt.toSwitchUses
        | Bp64 bp -> bp |> SwitchEventRolloutBp64.toSwitchUses