namespace global
open System


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