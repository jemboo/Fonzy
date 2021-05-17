namespace global
open System

type switchEventRolloutInt = {
            switchCount:SwitchCount; 
            sortableCount:SortableCount; 
            useRoll:IntBits}

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



type switchEventRolloutBp32 = {
    switchCount:SwitchCount; 
    sortableCount:SortableCount;
    sortableBlockCount:int;
    useRoll:bitsP32 }

module SwitchEventRolloutBp32 =
    let create (switchCount:SwitchCount) 
               (sortableCount:SortableCount) = 

        let ur = BitsP32.zeroSubCreate
                              ((SwitchCount.value switchCount) * 
                               (SortableCount.value sortableCount)) 
        {   switchCount=switchCount;
            sortableCount=sortableCount;
            sortableBlockCount=ur.values.Length;
            useRoll = ur }

    let toSwitchUses (switchEvents:switchEventRolloutBp32) =
        let switchCt = (SwitchCount.value switchEvents.switchCount)

        {   SwitchUses.switchCount = switchEvents.switchCount;
            SwitchUses.weights = Array.zeroCreate switchCt }



type switchEventRolloutBp64 = {
        switchCount:SwitchCount;
        sortableCount:SortableCount;
        sortableBlockCount:int;
        useRoll:bitsP64 }


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