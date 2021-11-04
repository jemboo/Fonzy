namespace global
open System

type sorterMutTypeDto = {cat:string; value:stringMapDto}
module SorterMutTypeDto =

    let fromDto (dto:sorterMutTypeDto) =
        match dto.cat with
        | nameof sorterMutType.BySwitch -> 
                   result {
                       let! b = dto.value |> StringMapDto.fromDto
                       let! pfxCtv = b.["pfxCount"] |> Json.deserialize<int>
                       let! pfxCt = pfxCtv |> SwitchCount.create ""
                       let! mrv = b.["mutationRate"] |> Json.deserialize<float>
                       let! mr = mrv |> MutationRate.create "" 
                       return sorterMutType.BySwitch (pfxCt, mr)
                   }
        | nameof sorterMutType.ByStage -> 
                    result {
                        let! b = dto.value |> StringMapDto.fromDto
                        let! pfxCtv = b.["pfxCount"] |> Json.deserialize<int>
                        let! pfxCt = pfxCtv |> SwitchCount.create ""
                        let! mrv = b.["mutationRate"] |> Json.deserialize<float>
                        let! mr = mrv |> MutationRate.create "" 
                        return sorterMutType.ByStage (pfxCt, mr)
                    }
        | nameof sorterMutType.ByStageRfl -> 
                    result {
                        let! b = dto.value |> StringMapDto.fromDto
                        let! pfxCtv = b.["pfxCount"] |> Json.deserialize<int>
                        let! pfxCt = pfxCtv |> SwitchCount.create ""
                        let! mrv = b.["mutationRate"] |> Json.deserialize<float>
                        let! mr = mrv |> MutationRate.create "" 
                        return sorterMutType.ByStageRfl (pfxCt, mr)
                    }
        | t -> (sprintf "Invalid sorterMutationTypeDto: %s" t) |> Error


    let fromJson (js:string) =
        result {
            let! dto = Json.deserialize<sorterMutTypeDto> js
            return! fromDto dto
        }

    let toDto (mutationType:sorterMutType) =
        let makePrams pfx mr = 
                [
                     ("pfxCount", pfx |> SwitchCount.value |> Json.serialize);
                     ("mutationRate", (MutationRate.value mr) |> Json.serialize);
                ] |> Map.ofList

        match mutationType with
        | sorterMutType.BySwitch (pfx, mr) ->  
            {
                sorterMutTypeDto.cat = nameof sorterMutType.BySwitch; 
                value = (makePrams pfx mr) |> StringMapDto.toDto;
            }
        | sorterMutType.ByStage (pfx, mr) ->
            {
                sorterMutTypeDto.cat = nameof sorterMutType.ByStage; 
                value =  (makePrams pfx mr) |> StringMapDto.toDto;   
            }

        | sorterMutType.ByStageRfl (pfx, mr) -> 
            {
                sorterMutTypeDto.cat = nameof sorterMutType.ByStageRfl; 
                value = (makePrams pfx mr) |> StringMapDto.toDto;  
            }

    let toJson (ss:sorterMutType) =
        ss |> toDto |> Json.serialize


type sorterRndGenDto = {cat:string; prams:stringMapDto; switches:int[]} 
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
            {
                sorterRndGenDto.cat = nameof sorterRndGen.RandSwitches; 
                prams = prams |> StringMapDto.toDto; 
                switches=switchListDto; 
            } 

        | sorterRndGen.RandStages (switchList, tc, d) ->
            let prams = 
                [
                    ("stageCount", (StageCount.value tc) |> string);
                    ("degree", (Degree.value d) |> string);
                ] |> Map.ofList                
            let switchListDto = switchList |> List.toArray 
                                    |> Array.map(fun sw -> SwitchDto.toDto sw)
            { 
                sorterRndGenDto.cat = nameof sorterRndGen.RandStages; 
                prams = prams |> StringMapDto.toDto; 
                switches=switchListDto; 
            } 

        | sorterRndGen.RandBuddies (switchList, tc, ws, d) ->
            let prams = 
                [
                    ("stageCount", (StageCount.value tc) |> string);
                    ("windowSize", (StageWindowSize.value ws) |> string);
                    ("degree", (Degree.value d) |> string);
                ] |> Map.ofList
            let switchListDto = switchList |> List.toArray 
                                |> Array.map(fun sw -> SwitchDto.toDto sw)
            { 
                sorterRndGenDto.cat = nameof sorterRndGen.RandBuddies; 
                prams = prams |> StringMapDto.toDto;
                switches=switchListDto; 
            }

        | sorterRndGen.RandSymmetric (switchList, tc, d) ->
            let prams = 
                [
                    ("stageCount", (StageCount.value tc) |> string);
                    ("degree", (Degree.value d) |> string);
                ] |> Map.ofList            
            let switchListDto = switchList |> List.toArray 
                                |> Array.map(fun sw -> SwitchDto.toDto sw)
            { 
                sorterRndGenDto.cat = nameof sorterRndGen.RandSymmetric; 
                prams = prams |> StringMapDto.toDto;
                switches=switchListDto; 
            } 

        | sorterRndGen.RandRflBuddies (switchList, tc, ws, d) ->
            let prams =
                [
                    ("stageCount", (StageCount.value tc) |> string);
                    ("windowSize", (StageWindowSize.value ws) |> string);
                    ("degree", (Degree.value d) |> string);
                ] |> Map.ofList
            let switchListDto = switchList |> List.toArray 
                                |> Array.map(fun sw -> SwitchDto.toDto sw)
            { 
                sorterRndGenDto.cat = nameof sorterRndGen.RandRflBuddies; 
                prams = prams |> StringMapDto.toDto;  
                switches=switchListDto; 
            } 


    let toJson (cs:sorterRndGen) =
        cs |> toDto |> Json.serialize

    let fromDto (sgDto:sorterRndGenDto) =
            match sgDto.cat with
            | nameof sorterRndGen.RandSwitches -> 
             result {
                let! map = sgDto.prams |> StringMapDto.fromDto; 
                let! degree = map |> ResultMap.procKeyedInt "degree" 
                                             (fun d -> Degree.create "" d)
                let! switchCount = map |> ResultMap.procKeyedInt "switchCount" 
                                            (fun d -> SwitchCount.create "" d)

                let! switchList = sgDto.switches |> Array.map(fun sw -> SwitchDto.fromDto sw)
                                                |> Array.toList
                                                |> Result.sequence
                return sorterRndGen.RandSwitches (switchList, switchCount, degree)
             }

            | nameof sorterRndGen.RandStages -> 
             result {
                    let! map = sgDto.prams |> StringMapDto.fromDto; 
                    let! stageCount = map |> ResultMap.procKeyedInt "stageCount" 
                                                (fun d -> StageCount.create "" d)
                    let! degree = map |> ResultMap.procKeyedInt "degree" 
                                                (fun d -> Degree.create "" d)

                    let! switchList = sgDto.switches |> Array.map(fun sw -> SwitchDto.fromDto sw)
                                                    |> Array.toList
                                                    |> Result.sequence

                    return sorterRndGen.RandStages (switchList, stageCount, degree)
             }

            | nameof sorterRndGen.RandBuddies -> 
             result {
                    let! map = sgDto.prams |> StringMapDto.fromDto; 
                    let! degree = map |> ResultMap.procKeyedInt "degree" 
                                                (fun d -> Degree.create "" d)
                    let! stageCount = map |> ResultMap.procKeyedInt "stageCount" 
                                                (fun d -> StageCount.create "" d) 
                    let! windowSize = map |> ResultMap.procKeyedInt "windowSize" 
                                                (fun d -> StageWindowSize.create "" d)
                    let! switchList = sgDto.switches |> Array.map(fun sw -> SwitchDto.fromDto sw)
                                                    |> Array.toList
                                                    |> Result.sequence
                    return sorterRndGen.RandBuddies (switchList, stageCount, windowSize, degree)
             }

            | nameof sorterRndGen.RandSymmetric -> 
             result {
                    let! map = sgDto.prams |> StringMapDto.fromDto; 
                    let! stageCount = map |> ResultMap.procKeyedInt "stageCount" 
                                                (fun d -> StageCount.create "" d)
                    let! degree = map |> ResultMap.procKeyedInt "degree" 
                                                (fun d -> Degree.create "" d)
                    let! switchList = sgDto.switches |> Array.map(fun sw -> SwitchDto.fromDto sw)
                                                    |> Array.toList
                                                    |> Result.sequence
                    return sorterRndGen.RandSymmetric (switchList, stageCount, degree)
             }
                           
            | nameof sorterRndGen.RandRflBuddies -> 
             result {
                    let! map = sgDto.prams |> StringMapDto.fromDto; 
                    let! degree = map |> ResultMap.procKeyedInt "degree" 
                                                (fun d -> Degree.create "" d)
                    let! stageCount = map |> ResultMap.procKeyedInt "stageCount" 
                                                (fun d -> StageCount.create "" d) 
                    let! windowSize = map |> ResultMap.procKeyedInt "windowSize" 
                                                (fun d -> StageWindowSize.create "" d)
                    let! switchList = sgDto.switches |> Array.map(fun sw -> SwitchDto.fromDto sw)
                                                    |> Array.toList
                                                    |> Result.sequence
                    return sorterRndGen.RandRflBuddies (switchList, stageCount, windowSize, degree)
             }

            | _ -> Error (sprintf "no match for SorterGenDto.cat: %s" sgDto.cat)


    let fromJson (js:string) =
        result {
            let! dto = Json.deserialize<sorterRndGenDto> js
            return! fromDto dto
        }

//type sorterMutationSpecDto = {sorterMutType:sorterMutationTypeDto; 
//                              rndGen:rngGenDto}
//module SorterMutationSpecDto =

//    let fromDto (dto:sorterMutationSpecDto) =
//        result {
//            let! smt = dto.sorterMutType |> SorterMutTypeDto.fromDto
//            let! rndg = dto.rndGen |> RngGenDto.fromDto
//            return sorterMutationSpec (smt, rndg);
//        }

//    let toDto (mutationSpec:sorterMutationSpec) =
//        let mutationType, rngGen = mutationSpec
//        {
//           sorterMutType = mutationType |> SorterMutTypeDto.toDto;
//           rndGen = rngGen |> RngGenDto.toDto;
//        }