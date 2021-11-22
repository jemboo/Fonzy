
namespace global

open System


type sorterPerfDto = {switches:int; stages:int; successful:bool option}
module SorterPerfDto =
    let fromDto (dto:sorterPerfDto) =
        result {
            let! tc = StageCount.create "" dto.stages
            let! wc = SwitchCount.create "" dto.switches
            return { SortingEval.sorterPerf.usedStageCount = tc;
                     SortingEval.sorterPerf.usedSwitchCount = wc;
                     SortingEval.sorterPerf.successful = dto.successful}
        }

    let fromJson (jstr:string) =
        result {
            let! dto = Json.deserialize<sorterPerfDto> jstr
            return! fromDto dto
        }

    let toDto (perf:SortingEval.sorterPerf) =
        {switches = (SwitchCount.value perf.usedSwitchCount); 
         stages= (StageCount.value perf.usedStageCount);
         successful = perf.successful}

    let toJson (perf:SortingEval.sorterPerf) =
        perf |> toDto |> Json.serialize


type sorterPerfBinDto = int[]
module SorterPerfBinDto =
    let toDto (spb:SortingEval.sorterPerfBin) =
        [|
            (SwitchCount.value spb.usedSwitchCount)
            (StageCount.value spb.usedStageCount)
            (SorterCount.value spb.sorterCount)
            spb.successCount
            spb.failCount
        |]

    let toTup (dto:sorterPerfBinDto) =
        result {
            let! uwc = SwitchCount.create "" dto.[0]
            let! utc = StageCount.create "" dto.[1]
            let! stc = SorterCount.create "" dto.[2]
            return
                { 
                    SortingEval.sorterPerfBin.usedSwitchCount = uwc;
                    SortingEval.sorterPerfBin.usedStageCount = utc;
                    SortingEval.sorterPerfBin.sorterCount = stc;
                    SortingEval.sorterPerfBin.successCount = dto.[3]
                    SortingEval.sorterPerfBin.failCount = dto.[4]
                }
        }

    let fromDtos (dtos:sorterPerfBinDto[]) =
        result {
                let! tups = dtos |> Array.map(toTup)
                                 |> Array.toList
                                 |> Result.sequence

                return tups |> List.toArray
            }

    let toDtos (perfBins:SortingEval.sorterPerfBin[]) =
        perfBins |> Array.map(toDto)

    let toJson (perfBins:SortingEval.sorterPerfBin[]) =
        perfBins |> toDtos |> Json.serialize

    let fromJson (json:string) =
        result {
            let! dto = Json.deserialize<sorterPerfBinDto[]> json
            return! dto |> fromDtos
        }



type sorterCoverageDto = {sorterId:Guid; 
                          perfDto:sorterPerfDto;
                          usedSwitches:int[]}

module SorterCoverageDto =
    let fromDto (dto:sorterCoverageDto) =
        result {
            let! sorterId = SorterId.create dto.sorterId
            let! perf = dto.perfDto |> SorterPerfDto.fromDto
            let! usedSwitches = dto.usedSwitches 
                                |> Array.map(fun sw -> SwitchDto.fromDto sw)
                                |> Array.toList
                                |> Result.sequence

            return { SortingEval.sorterCoverage.sorterId = sorterId;
                     SortingEval.sorterCoverage.perf = perf;
                     SortingEval.sorterCoverage.usedSwitches = 
                            usedSwitches |> List.toArray;}
        }

    let fromJson (jstr:string) =
        result {
            let! dto = Json.deserialize<sorterCoverageDto> jstr
            return! fromDto dto
        }

    let toDto (cov:SortingEval.sorterCoverage) =
        {sorterId = (SorterId.value cov.sorterId); 
        // sortableSetId = (SortableSetId.value cov.sortableSetId);
         perfDto = cov.perf |> SorterPerfDto.toDto;
         usedSwitches = cov.usedSwitches |> Array.map(SwitchDto.toDto)}

    let toJson (perf:SortingEval.sorterCoverage) =
        perf |> toDto |> Json.serialize





type sorterSavingDto =
    { savingType: string 
      data: string }
      
module SorterSavingDto =

    let fromDto (e:sorterSavingDto) =
        match e.savingType with
        | nameof sorterSaving.NotAny -> sorterSaving.NotAny  |> Ok
        | nameof sorterSaving.All -> sorterSaving.All |> Ok
        | nameof sorterSaving.Successful -> sorterSaving.Successful |> Ok
        | nameof sorterSaving.Perf -> 
            result {
                let! sw, sc = (Json.deserialize<float*int> e.data)
                return (StageWeight.fromFloat sw, SorterCount.fromInt sc)
                    |> sorterSaving.Perf
            }
        | t -> (sprintf "Invalid sorterSavingType: %s" t) |> Error

    let fromJson (js:string) =
        result {
            let! dto = Json.deserialize<sorterSavingDto> js
            return! fromDto dto
        }

    let toDto (ss:sorterSaving) =
        match ss with
        | sorterSaving.NotAny -> {sorterSavingDto.savingType = nameof sorterSaving.NotAny;
                                data = ""}
        | sorterSaving.All -> {sorterSavingDto.savingType = nameof sorterSaving.All;
                                data = ""}
        | sorterSaving.Successful -> {sorterSavingDto.savingType = nameof sorterSaving.Successful;
                                data = ""}
        | sorterSaving.Perf (sw,sc) -> {sorterSavingDto.savingType = nameof sorterSaving.Perf;
                                data = Json.serialize (StageWeight.value sw, SorterCount.value sc)}

    let toJson (ss:sorterSaving) =
        ss |> toDto |> Json.serialize
