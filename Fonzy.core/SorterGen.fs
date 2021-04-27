namespace global
open System

type SwitchMapTracker = {degree:Degree; tracker:int[] }

module SwitchMapTracker =
    let create (degree:Degree) = 
        let trackArraySize = int (Switch.switchCountForDegree degree)
        {SwitchMapTracker.degree = degree;
         tracker = Array.zeroCreate trackArraySize}

    let recordSwitches (recVal:int) 
                       (switchMapTracker:SwitchMapTracker) 
                       (switches:Switch seq) = 
        switches |> Seq.iter(fun w -> 
                switchMapTracker.tracker.[Switch.getIndex w] <- recVal)
        switchMapTracker

    let recordSwitch (recVal:int) 
                     (switchMapTracker:SwitchMapTracker) 
                     (switch:Switch) = 
        let dex = Switch.getIndex switch
        if switchMapTracker.tracker.[dex] = 0 then
           switchMapTracker.tracker.[dex] <- recVal
           true
        else false

    let usedIndexes (switchMapTracker:SwitchMapTracker) =
        switchMapTracker.tracker
        |> Array.mapi(fun d v -> (d,v))
        |> Array.filter(fun tup -> (snd tup) > 0)
        |> Array.map(fst)
        |> Array.toList

    let ageTracker (switchMapTracker:SwitchMapTracker) =
        {
            SwitchMapTracker.degree = switchMapTracker.degree;
            tracker = switchMapTracker.tracker
                      |> Array.map(fun v -> v - 1)
        }

    let recordAndFilter (recVal:int) 
                        (switchMapTracker:SwitchMapTracker) 
                        (switches:Switch seq) =
        seq { for w in switches do
                if recordSwitch recVal switchMapTracker w then
                    yield w }

type StageChoice = {tcpA:TwoCyclePerm; tcpB:TwoCyclePerm; aProb:float}
module StageChoice = 
    let create (permPair:TwoCyclePerm[]) =
        try
            {StageChoice.tcpA = permPair.[0];
             StageChoice.tcpB = permPair.[1];
             aProb = 0.5} |> Ok
        with 
        | ex -> Result.Error ex.Message

type SorterTreeBuilder = StageChoice List

module SorterTreeBuilder =
    let makeSorterTreeBuilder (tcps:TwoCyclePerm seq) 
                              (stageCount:StageCount) =
        tcps |> Seq.chunkBySize 2
             |> Seq.map(StageChoice.create)
             |> Seq.take (StageCount.value stageCount)
             |> Seq.toList
             |> Result.sequence

    let makeSorterTree (stageWindowSize:StageCount) 
                       (randy:IRando)  
                       (sorterTreeBuilder:SorterTreeBuilder) =
        None