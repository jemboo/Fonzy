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
            return Sorter.fromSwitches degree switches
        }
    let fromJson (cereal:string) =
        result {
            let! sorterDto = cereal |> Json.deserialize<SorterDto>
            return! fromDto sorterDto
        }
    let toDto (sorter:Sorter) =
        {
            SorterDto.degree = (Degree.value sorter.degree); 
            switches = sorter.switches |> Array.map(SwitchDto.toDto)
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
            return SwitchUses.init dto.weights
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
        | SorterGen.RandBuddies (tc, ws, d) -> 
            let prams = 
                [
                    ("stageCount", (StageCount.value tc) |> string);
                    ("windowSize", (StageCount.value ws) |> string);
                    ("degree", (Degree.value d) |> string);
                ] |> Map.ofList
            {SorterGenDto.cat="RandBuddies"; SorterGenDto.prams=prams} 
        | SorterGen.RandSymmetric (tc, d) -> 
            let prams = 
                [
                    ("stageCount", (StageCount.value tc) |> string);
                    ("degree", (Degree.value d) |> string);
                ] |> Map.ofList
            {SorterGenDto.cat="RandSymmetric"; SorterGenDto.prams=prams} 
        | SorterGen.RandSymmetricBuddies (tc, ws, d) -> 
            let prams = 
                [
                    ("stageCount", (StageCount.value tc) |> string);
                    ("windowSize", (StageCount.value ws) |> string);
                    ("degree", (Degree.value d) |> string);
                ] |> Map.ofList
            {SorterGenDto.cat="RandSymmetricBuddies"; SorterGenDto.prams=prams} 



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

            | "RandBuddies" -> 
                    result {
                            let! degree = sgDto.prams |> ResultMap.procKeyedInt "degree" 
                                                      (fun d -> Degree.create "" d)
                            let! stageCount = sgDto.prams |> ResultMap.procKeyedInt "stageCount" 
                                                      (fun d -> StageCount.create "" d) 
                            let! windowSize = sgDto.prams |> ResultMap.procKeyedInt "windowSize" 
                                                      (fun d -> StageCount.create "" d)
                            return SorterGen.RandBuddies (stageCount, windowSize, degree)
                           }
            | "RandSymmetric" -> 
                    result {
                            let! stageCount = sgDto.prams |> ResultMap.procKeyedInt "stageCount" 
                                                      (fun d -> StageCount.create "" d)
                            let! degree = sgDto.prams |> ResultMap.procKeyedInt "degree" 
                                                      (fun d -> Degree.create "" d)
                            return SorterGen.RandSymmetric (stageCount, degree)
                           }
                           
            | "RandSymmetricBuddies" -> 
                    result {
                            let! degree = sgDto.prams |> ResultMap.procKeyedInt "degree" 
                                                        (fun d -> Degree.create "" d)
                            let! stageCount = sgDto.prams |> ResultMap.procKeyedInt "stageCount" 
                                                        (fun d -> StageCount.create "" d) 
                            let! windowSize = sgDto.prams |> ResultMap.procKeyedInt "windowSize" 
                                                        (fun d -> StageCount.create "" d)
                            return SorterGen.RandSymmetricBuddies (stageCount, windowSize, degree)
                            }

            | _ -> Error (sprintf "no match for SorterGenDto.cat: %s" sgDto.cat)


    let fromJson (js:string) =
        result {
            let! dto = Json.deserialize<SorterGenDto> js
            return! fromDto dto
        }


type SorterRndGenDto = {cat:string; prams:Map<string,string>; switches:int[]} 
module SorterRndGenDto =
    let toDto (sg:sorterRndGen) =
        match sg with
        | sorterRndGen.RandSwitches (switchList, wc, d) ->
            let prams = 
                [
                    ("switchCount", (SwitchCount.value wc) |> string);
                    ("degree", (Degree.value d) |> string);
                ] |> Map.ofList
            let switchListDto = switchList |> List.toArray 
                                |> Array.map(fun sw -> SwitchDto.toDto sw)
            {SorterRndGenDto.cat="RandSwitches"; prams=prams; switches=switchListDto; } 
        | sorterRndGen.RandStages (switchList, tc, d) ->
            let prams = 
                [
                    ("stageCount", (StageCount.value tc) |> string);
                    ("degree", (Degree.value d) |> string);
                ] |> Map.ofList                
            let switchListDto = switchList |> List.toArray 
                                    |> Array.map(fun sw -> SwitchDto.toDto sw)
            { SorterRndGenDto.cat="RandStages"; prams=prams; switches=switchListDto; } 
        | sorterRndGen.RandBuddies (switchList, tc, ws, d) ->
            let prams = 
                [
                    ("stageCount", (StageCount.value tc) |> string);
                    ("windowSize", (StageWindowSize.value ws) |> string);
                    ("degree", (Degree.value d) |> string);
                ] |> Map.ofList
            let switchListDto = switchList |> List.toArray 
                                |> Array.map(fun sw -> SwitchDto.toDto sw)
            { SorterRndGenDto.cat="RandBuddies"; prams=prams; switches=switchListDto; } 
        | sorterRndGen.RandSymmetric (switchList, tc, d) ->
            let prams = 
                [
                    ("stageCount", (StageCount.value tc) |> string);
                    ("degree", (Degree.value d) |> string);
                ] |> Map.ofList            
            let switchListDto = switchList |> List.toArray 
                                |> Array.map(fun sw -> SwitchDto.toDto sw)
            { SorterRndGenDto.cat="RandSymmetric"; prams=prams; switches=switchListDto; } 
        | sorterRndGen.RandSymmetricBuddies (switchList, tc, ws, d) ->
            let prams =
                [
                    ("stageCount", (StageCount.value tc) |> string);
                    ("windowSize", (StageWindowSize.value ws) |> string);
                    ("degree", (Degree.value d) |> string);
                ] |> Map.ofList
            let switchListDto = switchList |> List.toArray 
                                |> Array.map(fun sw -> SwitchDto.toDto sw)
            { SorterRndGenDto.cat="RandSymmetricBuddies"; prams=prams; switches=switchListDto; } 



    let toJson (cs:sorterRndGen) =
        cs |> toDto |> Json.serialize

    let fromDto (sgDto:SorterRndGenDto) =
            match sgDto.cat with
            | "RandSwitches" -> 
                    result {
                            let! degree = sgDto.prams |> ResultMap.procKeyedInt "degree" 
                                                      (fun d -> Degree.create "" d)
                            let! switchCount = sgDto.prams |> ResultMap.procKeyedInt "switchCount" 
                                                      (fun d -> SwitchCount.create "" d)

                            let! switchList = sgDto.switches |> Array.map(fun sw -> SwitchDto.fromDto sw)
                                                           |> Array.toList
                                                           |> Result.sequence
                            return sorterRndGen.RandSwitches (switchList, switchCount, degree)
                           }

            | "RandStages" -> 
                    result {
                            let! stageCount = sgDto.prams |> ResultMap.procKeyedInt "stageCount" 
                                                      (fun d -> StageCount.create "" d)
                            let! degree = sgDto.prams |> ResultMap.procKeyedInt "degree" 
                                                      (fun d -> Degree.create "" d)

                            let! switchList = sgDto.switches |> Array.map(fun sw -> SwitchDto.fromDto sw)
                                                           |> Array.toList
                                                           |> Result.sequence

                            return sorterRndGen.RandStages (switchList, stageCount, degree)
                           }

            | "RandBuddies" -> 
                    result {
                            let! degree = sgDto.prams |> ResultMap.procKeyedInt "degree" 
                                                      (fun d -> Degree.create "" d)
                            let! stageCount = sgDto.prams |> ResultMap.procKeyedInt "stageCount" 
                                                      (fun d -> StageCount.create "" d) 
                            let! windowSize = sgDto.prams |> ResultMap.procKeyedInt "windowSize" 
                                                      (fun d -> StageWindowSize.create "" d)
                            let! switchList = sgDto.switches |> Array.map(fun sw -> SwitchDto.fromDto sw)
                                                           |> Array.toList
                                                           |> Result.sequence
                            return sorterRndGen.RandBuddies (switchList, stageCount, windowSize, degree)
                           }
            | "RandSymmetric" -> 
                    result {
                            let! stageCount = sgDto.prams |> ResultMap.procKeyedInt "stageCount" 
                                                      (fun d -> StageCount.create "" d)
                            let! degree = sgDto.prams |> ResultMap.procKeyedInt "degree" 
                                                      (fun d -> Degree.create "" d)
                            let! switchList = sgDto.switches |> Array.map(fun sw -> SwitchDto.fromDto sw)
                                                           |> Array.toList
                                                           |> Result.sequence
                            return sorterRndGen.RandSymmetric (switchList, stageCount, degree)
                           }
                           
            | "RandSymmetricBuddies" -> 
                    result {
                            let! degree = sgDto.prams |> ResultMap.procKeyedInt "degree" 
                                                        (fun d -> Degree.create "" d)
                            let! stageCount = sgDto.prams |> ResultMap.procKeyedInt "stageCount" 
                                                        (fun d -> StageCount.create "" d) 
                            let! windowSize = sgDto.prams |> ResultMap.procKeyedInt "windowSize" 
                                                        (fun d -> StageWindowSize.create "" d)
                            let! switchList = sgDto.switches |> Array.map(fun sw -> SwitchDto.fromDto sw)
                                                           |> Array.toList
                                                           |> Result.sequence
                            return sorterRndGen.RandSymmetricBuddies (switchList, stageCount, windowSize, degree)
                            }

            | _ -> Error (sprintf "no match for SorterGenDto.cat: %s" sgDto.cat)


    let fromJson (js:string) =
        result {
            let! dto = Json.deserialize<SorterRndGenDto> js
            return! fromDto dto
        }