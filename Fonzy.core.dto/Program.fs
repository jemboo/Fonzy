// Learn more about F# at http://fsharp.org

open System

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code


    
    
    //type SorterPoolEnviroDto = {cat:string; value:string}
    //module SorterPoolEnviroDto =
    //    let toDto (env:SorterPoolEnviro) =
    //         match env with
    //         | SorterPoolEnviro.Bag n -> {cat="Bag"; value = Json.serialize n}
    //         | SorterPoolEnviro.Torus n -> {cat="Torus"; value = Json.serialize n}
    
    
    //    let fromDto (eDto:SorterPoolEnviroDto) =
    //        if eDto.cat = "Bag" then
    //            result {
    //                let! b = Json.deserialize<int> eDto.value
    //                return SorterPoolEnviro.Bag b
    //            }
    //        else if eDto.cat = "Torus" then
    //            result {
    //                let! b = Json.deserialize<int> eDto.value
    //                return SorterPoolEnviro.Torus b
    //            }
    //        else sprintf "cat: %s for SorterPoolEnviroDto not found"
    //                      eDto.cat |> Error
    

    
    //type NumberOrgDto = 
    //    {
    //        numberOrgId:Guid
    //        numberOrgType:NumberOrgType
    //        parentId:Guid option
    //        generation:int
    //        floatPhenotype:float
    //        phenotypeEval:float
    //    }
    
    //module NumberOrgDto =
    
    //    let fromDto (dto:NumberOrgDto) =
    //        result 
    //            {
    //                let! numberOrgId = NumberOrgId.create dto.numberOrgId
    //                let numberOrgType = dto.numberOrgType
    //                let! parentId = match dto.parentId with
    //                                | Some guey -> NumberOrgId.create guey
    //                                               |> Result.map Some
    //                                | None -> None |> Ok
    //                let! generation = GenerationNumber.create "" dto.generation
    
    //                return {
    //                    NumberOrg.numberOrgId = numberOrgId
    //                    numberOrgType = numberOrgType
    //                    parentId = parentId
    //                    generation = generation
    //                    floatPhenotype = dto.floatPhenotype
    //                    phenotypeEval = dto.phenotypeEval
    //                }
    //            }
    
    
    //    let toDto (numberOrg:NumberOrg) =
    //        {
    //            NumberOrgDto.numberOrgId = NumberOrgId.value numberOrg.numberOrgId
    //            numberOrgType = numberOrg.numberOrgType
    //            parentId = match numberOrg.parentId with
    //                        | Some id -> Some (NumberOrgId.value id)
    //                        | None -> None
    //            generation = numberOrg.generation |> GenerationNumber.value
    //            floatPhenotype = numberOrg.floatPhenotype
    //            phenotypeEval = numberOrg.phenotypeEval
    //        }
    
    //type NumberOrgsDto = 
    //    {
    //        id:Guid
    //        numberOrgs:NumberOrgDto[]
    //    }
    
    //module NumberOrgsDto =
    
    //    let fromDto (dto:NumberOrgsDto) =
    //        result 
    //            {
    //                let! orgList = dto.numberOrgs
    //                                |> Array.map(NumberOrgDto.fromDto)
    //                                |> Array.toList |> Result.sequence
    
    //                let orgMap = orgList |> List.map(fun gl-> (gl.numberOrgId, gl))
    //                                     |> Map.ofList
    
    //                return {
    //                    NumberOrgs.id = dto.id
    //                    NumberOrgs.orgMap = orgMap
    //                }
    //            }
    
    //    let toDto (numberOrgs:NumberOrgs) =
    //        {
    //            NumberOrgsDto.id = numberOrgs.id
    //            NumberOrgsDto.numberOrgs = numberOrgs.orgMap
    //                            |> Map.toArray
    //                            |> Array.map(fun tup-> snd tup)
    //                            |> Array.map(NumberOrgDto.toDto)
    //        }
            
    //    type NumberOrgsDto = 
    //        {
    //            id:Guid
    //            numberOrgs:NumberOrgDto[]
    //        }
            
    //    module NumberOrgsDto =
            
    //        let fromDto (dto:NumberOrgsDto) =
    //            result 
    //                {
    //                    let! orgList = dto.numberOrgs
    //                                    |> Array.map(NumberOrgDto.fromDto)
    //                                    |> Array.toList |> Result.sequence
            
    //                    let orgMap = orgList |> List.map(fun gl-> (gl.numberOrgId, gl))
    //                                            |> Map.ofList
            
    //                    return {
    //                        NumberOrgs.id = dto.id
    //                        NumberOrgs.orgMap = orgMap
    //                    }
    //                }
            
    //        let toDto (numberOrgs:NumberOrgs) =
    //            {
    //                NumberOrgsDto.id = numberOrgs.id
    //                NumberOrgsDto.numberOrgs = numberOrgs.orgMap
    //                                |> Map.toArray
    //                                |> Array.map(fun tup-> snd tup)
    //                                |> Array.map(NumberOrgDto.toDto)
    //            }
    
    //type NumberOrgsWithGridLocsDto = 
    //    {
    //        id:Guid
    //        numberOrgs:NumberOrgDto[]
    //        poolOfGridLocationsDto:PoolOfGridLocationsDto
    //    }
    
    //module NumberOrgsWithGridLocsDto =
    
    //    let fromDto (dto:NumberOrgsWithGridLocsDto) =
    //        result 
    //            {
    //                let! orgList = dto.numberOrgs
    //                                |> Array.map(NumberOrgDto.fromDto)
    //                                |> Array.toList |> Result.sequence
    
    //                let orgMap = orgList |> List.map(fun gl-> (gl.numberOrgId, gl))
    //                                     |> Map.ofList
    //                let! poolOfGridLocations =  dto.poolOfGridLocationsDto |> PoolOfGridLocationsDto.fromDto
    
    //                return {
    //                    NumberOrgsWithGridLocs.id = dto.id
    //                    NumberOrgsWithGridLocs.orgMap = orgMap
    //                    NumberOrgsWithGridLocs.poolOfGridLocations = poolOfGridLocations
    //                }
    //            }
    
    //    let toDto (numberOrgs:NumberOrgsWithGridLocs) =
    //        {
    //            NumberOrgsWithGridLocsDto.id = numberOrgs.id
    //            NumberOrgsWithGridLocsDto.numberOrgs = numberOrgs.orgMap
    //                                        |> Map.toArray
    //                                        |> Array.map(fun tup-> snd tup)
    //                                        |> Array.map(NumberOrgDto.toDto)
    //            NumberOrgsWithGridLocsDto.poolOfGridLocationsDto = numberOrgs.poolOfGridLocations 
    //                                        |> PoolOfGridLocationsDto.toDto
    //        }
            
    
    //    type NumberOrgsDto = 
    //        {
    //            id:Guid
    //            numberOrgs:NumberOrgDto[]
    //        }
            
    //    module NumberOrgsDto =
            
    //        let fromDto (dto:NumberOrgsDto) =
    //            result 
    //                {
    //                    let! orgList = dto.numberOrgs
    //                                    |> Array.map(NumberOrgDto.fromDto)
    //                                    |> Array.toList |> Result.sequence
            
    //                    let orgMap = orgList |> List.map(fun gl-> (gl.numberOrgId, gl))
    //                                            |> Map.ofList
            
    //                    return {
    //                        NumberOrgs.id = dto.id
    //                        NumberOrgs.orgMap = orgMap
    //                    }
    //                }
            
    //        let toDto (numberOrgs:NumberOrgs) =
    //            {
    //                NumberOrgsDto.id = numberOrgs.id
    //                NumberOrgsDto.numberOrgs = numberOrgs.orgMap
    //                                |> Map.toArray
    //                                |> Array.map(fun tup-> snd tup)
    //                                |> Array.map(NumberOrgDto.toDto)
    //            }
    
    
    //type NumberPoolEnviroDto = {cat:string; value:string}
        
    //module NumberPoolEnviroDto =
        
    //    let fromDto (dto:NumberPoolEnviroDto) =
    //        if dto.cat = "Bag" then
    //            result {
    //                let! b = Json.deserialize<NumberOrgs> dto.value
    //                return NumberPoolEnviro.createBag b
    //            }
    //        else if dto.cat = "Grid" then
    //            result {
    //                let! b = Json.deserialize<NumberOrgs> dto.value
    //                return NumberPoolEnviro.createBag b
    //            }
    
    //        else sprintf "cat: %s for NumberPoolEnviroDto not found"
    //                      dto.cat |> Error
    
    
    //    let toDto (numberPoolEnviro:NumberPoolEnviro) =
    //            match numberPoolEnviro with
    //                  | NumberPoolEnviro.Bag bag -> { NumberPoolEnviroDto.cat = "Bag";
    //                                                  NumberPoolEnviroDto.value = Json.serialize bag }
    //                  | NumberPoolEnviro.Grid grid -> { NumberPoolEnviroDto.cat = "Grid";
    //                                                    NumberPoolEnviroDto.value = Json.serialize grid }
    
    






















//module OrgsEnviromentDto =

//    let fromDto (dto:OrgsWithGridLocsDto) =
//        result 
//            {
//                let! orgList = dto.orgs
//                                |> Array.map(OrgDto.fromDto)
//                                |> Array.toList |> Result.sequence

//                let orgMap = orgList |> List.map(fun gl-> (gl.orgId, gl))
//                                     |> Map.ofList
//                let! poolOfGridLocations =  dto.poolOfGridLocationsDto |> PoolOfGridLocationsDto.fromDto

//                return {
//                    OrgsWithGridLocs.id = dto.id
//                    OrgsWithGridLocs.orgMap = orgMap
//                    OrgsWithGridLocs.poolOfGridLocations = poolOfGridLocations
//                }
//            }

//    let toDto (orgs:OrgsWithGridLocs) =
//        {
//            OrgsWithGridLocsDto.id = orgs.id
//            OrgsWithGridLocsDto.orgs = orgs.orgMap
//                                        |> Map.toArray
//                                        |> Array.map(fun tup-> snd tup)
//                                        |> Array.map(OrgDto.toDto)
//            OrgsWithGridLocsDto.poolOfGridLocationsDto = orgs.poolOfGridLocations 
//                                        |> PoolOfGridLocationsDto.toDto
//        }
    

    //type OrgsDto = 
    //    {
    //        id:Guid
    //        orgs:OrgDto[]
    //    }
    
    //module OrgsDto =
    
    //    let fromDto (dto:OrgsDto) =
    //        result 
    //            {
    //                let! orgList = dto.orgs
    //                                |> Array.map(OrgDto.fromDto)
    //                                |> Array.toList |> Result.sequence
    
    //                let orgMap = orgList |> List.map(fun gl-> (gl.orgId, gl))
    //                                        |> Map.ofList
    
    //                return {
    //                    Orgs.id = dto.id
    //                    Orgs.orgMap = orgMap
    //                }
    //            }
    
    //    let toDto (orgs:Orgs) =
    //        {
    //            OrgsDto.id = orgs.id
    //            OrgsDto.orgs = orgs.orgMap
    //                            |> Map.toArray
    //                            |> Array.map(fun tup-> snd tup)
    //                            |> Array.map(OrgDto.toDto)
    //        }
    



//type OrgPoolEnviroDto = {cat:string; value:string}
//module OrgPoolEnviroDto =
//    let toDto (env:OrgPoolEnviro) =
//         match env with
//         | OrgPoolEnviro.Bag ob -> {cat="Bag"; value = ob |> OrgsDto.toDto  |> Json.serialize}
//         | OrgPoolEnviro.Grid og -> {cat="Grid"; value = og |> OrgsWithGridLocsDto.toDto  |> Json.serialize}


//    let fromDto (eDto:OrgPoolEnviroDto) =
//        if eDto.cat = "Bag" then
//            result {
//                let! dto = Json.deserialize<OrgsDto> eDto.value
//                let! ob = dto |> OrgsDto.fromDto
//                return OrgPoolEnviro.Bag ob
//            }
//        else if eDto.cat = "Grid" then
//            result {
//                let! dto = Json.deserialize<OrgsWithGridLocsDto> eDto.value
//                let! ogl = dto |> OrgsWithGridLocsDto.fromDto
//                return OrgPoolEnviro.Grid ogl
//            }
//        else sprintf "cat: %s for OrgPoolEnviroDto not found"
//                      eDto.cat |> Error
