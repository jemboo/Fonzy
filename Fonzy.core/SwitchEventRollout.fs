namespace global
open System

type switchEventRolloutInt = {
            switchCount:SwitchCount; 
            sortableCount:SortableCount; 
            useRoll:IntBits}


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
        {   switchCount=switchCount;
            sortableCount=sortableCount;
            useRoll = IntBits.zeroCreate 
                        ((SwitchCount.value switchCount) * 
                        (SortableCount.value sortableCount))    }


    let toSwitchUses (switchEvents:switchEventRolloutInt) =
        let swCt = (SwitchCount.value switchEvents.switchCount)
        let useWeights = Array.zeroCreate swCt
        let upDateSwU dex v =
            let swUdex = dex % swCt
            useWeights.[swUdex] <- useWeights.[swUdex] + v

        switchEvents.useRoll.values |> Array.iteri(fun dex v -> upDateSwU dex v)

        {   SwitchUses.switchCount = switchEvents.switchCount;
            SwitchUses.weights = useWeights    }


module SwitchEventRolloutBp64 =
    let create (switchCount:SwitchCount) 
               (sortableCount:SortableCount) = 

        let ur = BitsP64.zeroSubCreate
                              ((SwitchCount.value switchCount) * 
                               (SortableCount.value sortableCount))

        {   switchCount = switchCount;
            sortableCount = sortableCount;
            sortableBlockCount = ur.values.Length;
            useRoll = ur }


    let toSwitchUses (switchEvents:switchEventRolloutBp64) =
        let switchCt = (SwitchCount.value switchEvents.switchCount)

        {   SwitchUses.switchCount = switchEvents.switchCount;
            SwitchUses.weights = Array.zeroCreate switchCt }
            


module SwitchEventRollout =
    let toSwitchUses (switchEventRollout:switchEventRollout) =
        match switchEventRollout with
        | Int si -> si |> SwitchEventRolloutInt.toSwitchUses
        | Bp64 bp -> bp |> SwitchEventRolloutBp64.toSwitchUses