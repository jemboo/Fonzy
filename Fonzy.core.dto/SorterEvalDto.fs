namespace global

type SorterPerfBinsDto = int[]
module SorterPerfBinsDto =
    let toDto (tup:SortingEval.SorterPerfBin*int) =
        let perfBin = fst tup
        [|
            (SwitchCount.value perfBin.usedSwitchCount)
            (StageCount.value perfBin.usedStageCount)
            snd tup
        |]

    let toTup (dto:SorterPerfBinsDto) =
        result {
            let! uwc = SwitchCount.create "" dto.[0]
            let! utc = StageCount.create "" dto.[1]
            return (  
                      { 
                        SortingEval.SorterPerfBin.usedSwitchCount = uwc
                        SortingEval.SorterPerfBin.usedStageCount = utc
                      },
                      dto.[2]
                   )
         }

    let fromDtos (dtos:SorterPerfBinsDto[]) =
        result {
                let! tups = dtos |> Array.map(toTup)
                                 |> Array.toList
                                 |> Result.sequence

                return tups |> List.toArray
            }

    let toDtos (perfBins:(SortingEval.SorterPerfBin*int)[]) =
        perfBins |> Array.map(toDto)

    let toJson (perfBins:(SortingEval.SorterPerfBin*int)[]) =
        perfBins |> toDtos |> Json.serialize

    let fromJson (json:string) =
        result {
            let! dto = Json.deserialize<SorterPerfBinsDto[]> json
            return! dto |> fromDtos
        }