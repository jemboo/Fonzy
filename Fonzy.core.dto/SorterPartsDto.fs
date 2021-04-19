namespace global
open System


module SwitchDto =
    let fromDto dex = Switch.switchMap.[dex] |> Ok
    let toDto (switch:Switch) =
        (switch.hi * (switch.hi + 1)) / 2 + switch.low
     

type SorterDto = {degree:int; switches:int[]}
module SorterDto =
    let fromDto (dto:SorterDto) =
        result {
            let! degree = Degree.create "" dto.degree
            let! switches = dto.switches |> Array.map(fun sw -> SwitchDto.fromDto sw)
                                        |> Array.toList
                                        |> Result.sequence
            return Sorter.create degree switches
        }
    let fromJson (cereal:string) =
        result {
            let! sorterDto = cereal |> Json.deserialize<SorterDto>
            return! fromDto sorterDto
        }
    let toDto (sorter:Sorter) =
        {
            SorterDto.degree = (Degree.value sorter.degree); 
            switches = sorter.switches |> Array.map(fun sw -> SwitchDto.toDto sw)
        }
    let toJson (sorter:Sorter) =
        sorter |> toDto |> Json.serialize


type SorterSetDto = {id:Guid; degree:int; sorterDtos:SorterDto[]}
module SorterSetDto =
    let fromDto (dto:SorterSetDto) =
        result {
            let! sorterSetId = dto.id |> SorterSetId.create
            let! degree = dto.degree |> Degree.create ""
            let! sorters = dto.sorterDtos |> Array.map(SorterDto.fromDto)
                                          |> Array.toList
                                          |> Result.sequence
            return SorterSet.fromSorters sorterSetId degree sorters 
        }
    let fromJson (cereal:string) =
        result {
            let! sorterDto = cereal |> Json.deserialize<SorterSetDto>
            return! fromDto sorterDto
        }
    let toDto (sorterSet:SorterSet) =
        {
            SorterSetDto.id = (SorterSetId.value sorterSet.id)
            SorterSetDto.sorterDtos = sorterSet.sorters 
                                        |> Map.toArray
                                        |> Array.map(fun kvp -> 
                            (kvp |> snd |> SorterDto.toDto))
            SorterSetDto.degree = (Degree.value sorterSet.degree)
        }
    let toJson (sorters:SorterSet) =
        sorters |> toDto |> Json.serialize


type SwitchUsesDto = {switchCount:int; weights:int[]}
module SwitchUsesDto =
    let fromDto (dto:SwitchUsesDto) =
        result {
            let! switchCount = SwitchCount.create "" dto.switchCount
            return! SwitchUses.create switchCount dto.weights
        }

    let fromJson (cereal:string) =
        result {
            let! sorterDto = cereal |> Json.deserialize<SwitchUsesDto>
            return! fromDto sorterDto
        }

    let toDto (switchUses:SwitchUses) =
        {SwitchUsesDto.switchCount= (SwitchUses.switchCount switchUses); 
         weights = (SwitchUses.getWeights switchUses)}

    let toJson (switchUses:SwitchUses) =
        switchUses |> toDto |> Json.serialize

type SorterGenDto = {cat:string; prams:Map<string,string>} 
module SorterGenDto =
    let toDto (sg:SorterGen) =
        match sg with
        | SorterGen.RandSwitches (wc, d) ->
            let prams = 
                [
                    ("switchCount", (SwitchCount.value wc) |> string);
                    ("degree", (Degree.value d) |> string);
                ] |> Map.ofList
            {SorterGenDto.cat="RandSwitches"; SorterGenDto.prams=prams} 
        | SorterGen.RandStages (tc, d) -> 
            let prams = 
                [
                    ("stageCount", (StageCount.value tc) |> string);
                    ("degree", (Degree.value d) |> string);
                ] |> Map.ofList
            {SorterGenDto.cat="RandStages"; SorterGenDto.prams=prams} 
        | SorterGen.RandCoComp (tc, d) -> 
            let prams = 
                [
                    ("stageCount", (StageCount.value tc) |> string);
                    ("degree", (Degree.value d) |> string);
                ] |> Map.ofList
            {SorterGenDto.cat="RandCoComp"; SorterGenDto.prams=prams}
        | SorterGen.RandTriComp (tc, d) -> 
            let prams = 
                [
                    ("stageCount", (StageCount.value tc) |> string);
                    ("degree", (Degree.value d) |> string);
                ] |> Map.ofList
            {SorterGenDto.cat="RandTriComp"; SorterGenDto.prams=prams} 


    let toJson (cs:SorterGen) =
        cs |> toDto |> Json.serialize

    let fromDto (sgDto:SorterGenDto) =
            match sgDto.cat with
            | "RandSwitches" -> 
                    result {
                            let! degree = sgDto.prams |> ResultMap.procKeyedInt "degree" 
                                                      (fun d -> Degree.create "" d)
                            let! switchCount = sgDto.prams |> ResultMap.procKeyedInt "switchCount" 
                                                      (fun d -> SwitchCount.create "" d)
                            return SorterGen.RandSwitches (switchCount, degree)
                           }

            | "RandStages" -> 
                    result {
                            let! stageCount = sgDto.prams |> ResultMap.procKeyedInt "stageCount" 
                                                      (fun d -> StageCount.create "" d)
                            let! degree = sgDto.prams |> ResultMap.procKeyedInt "degree" 
                                                      (fun d -> Degree.create "" d)
                            return SorterGen.RandStages (stageCount, degree)
                           }
            | "RandCoComp" -> 
                    result {
                            let! degree = sgDto.prams |> ResultMap.procKeyedInt "degree" 
                                                      (fun d -> Degree.create "" d)
                            let! stageCount = sgDto.prams |> ResultMap.procKeyedInt "stageCount" 
                                                      (fun d -> StageCount.create "" d)
                            return SorterGen.RandCoComp (stageCount, degree)
                           }

            | "RandTriComp" -> 
                    result {
                            let! degree = sgDto.prams |> ResultMap.procKeyedInt "degree" 
                                                      (fun d -> Degree.create "" d)
                            let! stageCount = sgDto.prams |> ResultMap.procKeyedInt "stageCount" 
                                                      (fun d -> StageCount.create "" d)
                            return SorterGen.RandCoComp (stageCount, degree)
                           }
            | _ -> Error (sprintf "no match for SorterGenDto.cat: %s" sgDto.cat)


    let fromJson (js:string) =
        result {
            let! dto = Json.deserialize<SorterGenDto> js
            return! fromDto dto
        }