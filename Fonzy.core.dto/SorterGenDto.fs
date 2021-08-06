namespace global
open System


type sorterMutationTypeDto = {cat:string; value:string}
module SorterMutationTypeDto =

    let fromDto dto =
        if dto.cat = "BySwitch" then
            result {
                let! b = Json.deserialize<Map<string, string>> dto.value
                let! pfxCtv = b.["pfxCount"] |> Json.deserialize<int>
                let! pfxCt = pfxCtv |> SwitchCount.create ""
                let! mrv = b.["mutationRate"] |> Json.deserialize<float>
                let! mr = mrv |> MutationRate.create "" 
                return sorterMutationType.BySwitch (pfxCt, mr)
            }
        else if dto.cat = "ByStage" then
            result {
                let! b = Json.deserialize<Map<string, string>> dto.value
                let! pfxCtv = b.["pfxCount"] |> Json.deserialize<int>
                let! pfxCt = pfxCtv |> SwitchCount.create ""
                let! mrv = b.["mutationRate"] |> Json.deserialize<float>
                let! mr = mrv |> MutationRate.create "" 
                return sorterMutationType.ByStage (pfxCt, mr)
            }
        else sprintf "cat: %s for SorterMutationSpecDto not found"
                        dto.cat |> Error

    let toDto (mutationType:sorterMutationType) =
        let makePrams pfx mr = 
                [
                     ("pfxCount", pfx |> SwitchCount.value |> Json.serialize);
                     ("mutationRate", (MutationRate.value mr) |> Json.serialize);
                ] |> Map.ofList

        match mutationType with
        | sorterMutationType.BySwitch (pfx, mr) -> 
            {
                sorterMutationTypeDto.cat = "BySwitch"; 
                value = (makePrams pfx mr) |> Json.serialize;      
            }
        | sorterMutationType.ByStage (pfx, mr) ->
            {
                sorterMutationTypeDto.cat = "ByStage"; 
                value = (makePrams pfx mr) |> Json.serialize;    
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