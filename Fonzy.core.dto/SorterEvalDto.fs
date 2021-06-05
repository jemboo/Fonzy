namespace global

type SorterPerfDto = int[]
module SorterPerfBinsDto =
    let toDto (tup:SortingEval.SorterPerf*int) =
        let perfBin = fst tup
        [|
            (SwitchCount.value perfBin.usedSwitchCount)
            (StageCount.value perfBin.usedStageCount)
            snd tup
        |]

    let toTup (dto:SorterPerfDto) =
        result {
            let! uwc = SwitchCount.create "" dto.[0]
            let! utc = StageCount.create "" dto.[1]
            return (  
                      { 
                        SortingEval.SorterPerf.usedSwitchCount = uwc
                        SortingEval.SorterPerf.usedStageCount = utc
                        SortingEval.SorterPerf.sucessful = None
                      },
                      dto.[2]
                   )
         }

    let fromDtos (dtos:SorterPerfDto[]) =
        result {
                let! tups = dtos |> Array.map(toTup)
                                 |> Array.toList
                                 |> Result.sequence

                return tups |> List.toArray
            }

    let toDtos (perfBins:(SortingEval.SorterPerf*int)[]) =
        perfBins |> Array.map(toDto)

    let toJson (perfBins:(SortingEval.SorterPerf*int)[]) =
        perfBins |> toDtos |> Json.serialize

    let fromJson (json:string) =
        result {
            let! dto = Json.deserialize<SorterPerfDto[]> json
            return! dto |> fromDtos
        }