namespace global
open System

type sorterMutationTypeDto = {cat:string; value:string}
module SorterMutationTypeDto =

    let fromDto (dto:sorterMutationTypeDto) =
        match dto.cat with
        | nameof sorterMutationType.BySwitch -> 
                   result {
                       let! b = Json.deserialize<Map<string, string>> dto.value
                       let! pfxCtv = b.["pfxCount"] |> Json.deserialize<int>
                       let! pfxCt = pfxCtv |> SwitchCount.create ""
                       let! mrv = b.["mutationRate"] |> Json.deserialize<float>
                       let! mr = mrv |> MutationRate.create "" 
                       return sorterMutationType.BySwitch (pfxCt, mr)
                   }
        | nameof sorterMutationType.ByStage -> 
                    result {
                        let! b = Json.deserialize<Map<string, string>> dto.value
                        let! pfxCtv = b.["pfxCount"] |> Json.deserialize<int>
                        let! pfxCt = pfxCtv |> SwitchCount.create ""
                        let! mrv = b.["mutationRate"] |> Json.deserialize<float>
                        let! mr = mrv |> MutationRate.create "" 
                        return sorterMutationType.ByStage (pfxCt, mr)
                    }

        | t -> (sprintf "Invalid sorterMutationTypeDto: %s" t) |> Error

    let fromJson (js:string) =
        result {
            let! dto = Json.deserialize<sorterMutationTypeDto> js
            return! fromDto dto
        }

    let toDto (mutationType:sorterMutationType) =
        let makePrams pfx mr = 
                [
                     ("pfxCount", pfx |> SwitchCount.value |> Json.serialize);
                     ("mutationRate", (MutationRate.value mr) |> Json.serialize);
                ] |> Map.ofList

        match mutationType with
        | sorterMutationType.BySwitch (pfx, mr) -> 
            {
                sorterMutationTypeDto.cat = nameof sorterMutationType.BySwitch; 
                value = (makePrams pfx mr) |> Json.serialize;      
            }
        | sorterMutationType.ByStage (pfx, mr) ->
            {
                sorterMutationTypeDto.cat = nameof sorterMutationType.ByStage; 
                value = (makePrams pfx mr) |> Json.serialize;    
            }

    let toJson (ss:sorterMutationType) =
        ss |> toDto |> Json.serialize


type sorterRndGenDto = {cat:string; prams:Map<string,string>; switches:int[]} 
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
            {sorterRndGenDto.cat="RandSwitches"; prams=prams; switches=switchListDto; } 
        | sorterRndGen.RandStages (switchList, tc, d) ->
            let prams = 
                [
                    ("stageCount", (StageCount.value tc) |> string);
                    ("degree", (Degree.value d) |> string);
                ] |> Map.ofList                
            let switchListDto = switchList |> List.toArray 
                                    |> Array.map(fun sw -> SwitchDto.toDto sw)
            { sorterRndGenDto.cat="RandStages"; prams=prams; switches=switchListDto; } 
        | sorterRndGen.RandBuddies (switchList, tc, ws, d) ->
            let prams = 
                [
                    ("stageCount", (StageCount.value tc) |> string);
                    ("windowSize", (StageWindowSize.value ws) |> string);
                    ("degree", (Degree.value d) |> string);
                ] |> Map.ofList
            let switchListDto = switchList |> List.toArray 
                                |> Array.map(fun sw -> SwitchDto.toDto sw)
            { sorterRndGenDto.cat="RandBuddies"; prams=prams; switches=switchListDto; } 
        | sorterRndGen.RandSymmetric (switchList, tc, d) ->
            let prams = 
                [
                    ("stageCount", (StageCount.value tc) |> string);
                    ("degree", (Degree.value d) |> string);
                ] |> Map.ofList            
            let switchListDto = switchList |> List.toArray 
                                |> Array.map(fun sw -> SwitchDto.toDto sw)
            { sorterRndGenDto.cat="RandSymmetric"; prams=prams; switches=switchListDto; } 
        | sorterRndGen.RandSymmetricBuddies (switchList, tc, ws, d) ->
            let prams =
                [
                    ("stageCount", (StageCount.value tc) |> string);
                    ("windowSize", (StageWindowSize.value ws) |> string);
                    ("degree", (Degree.value d) |> string);
                ] |> Map.ofList
            let switchListDto = switchList |> List.toArray 
                                |> Array.map(fun sw -> SwitchDto.toDto sw)
            { sorterRndGenDto.cat="RandSymmetricBuddies"; prams=prams; switches=switchListDto; } 



    let toJson (cs:sorterRndGen) =
        cs |> toDto |> Json.serialize

    let fromDto (sgDto:sorterRndGenDto) =
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
            let! dto = Json.deserialize<sorterRndGenDto> js
            return! fromDto dto
        }

//type sorterMutationSpecDto = {sorterMutationType:sorterMutationTypeDto; 
//                              rndGen:rngGenDto}
//module SorterMutationSpecDto =

//    let fromDto (dto:sorterMutationSpecDto) =
//        result {
//            let! smt = dto.sorterMutationType |> SorterMutationTypeDto.fromDto
//            let! rndg = dto.rndGen |> RngGenDto.fromDto
//            return sorterMutationSpec (smt, rndg);
//        }

//    let toDto (mutationSpec:sorterMutationSpec) =
//        let mutationType, rngGen = mutationSpec
//        {
//           sorterMutationType = mutationType |> SorterMutationTypeDto.toDto;
//           rndGen = rngGen |> RngGenDto.toDto;
//        }