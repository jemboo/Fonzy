namespace global
open System

type JobssDto = {name:string}

module JobssDto = 
    let q = None



//type JobDto = {cat:string; value:string}
//module JobDto =
//    let toDto (j:Job) =
//         match j with
//         | Job.GetWorld n -> {cat="GetWorld"; value = n |> WorldDto.toDto |> Json.serialize}
//         | Job.MakeWorld n -> {cat="MakeWorld"; value = n |> WorldActionDto.toDto |> Json.serialize}


//    let fromDto (eDto:JobDto) =
//        if eDto.cat = "GetWorld" then
//            result {
//                let! dto = eDto.value |> Json.deserialize<WorldDto>
//                let! w = WorldDto.fromDto dto
//                return Job.GetWorld w
//            }
//        else if eDto.cat = "MakeWorld" then
//            result {
//                let! dto = eDto.value |> Json.deserialize<WorldActionDto>
//                let! wa = WorldActionDto.fromDto dto
//                return Job.MakeWorld wa
//            }
//        else sprintf "cat: %s for JobFileDto not found"
//                      eDto.cat |> Error


//type JobFile =
//    | ReadWorld of WorldDto
//    | MakeWorld of WorldActionDto

//module JobFile =
//    let toWorld (jobFile:JobFile) = 
//        match jobFile with
//        | ReadWorld dto -> 
//            result {
//                return! dto |> WorldDto.fromDto
//            }
//        | MakeWorld dto -> 
//            result {
//                let! wact = dto |> WorldActionDto.fromDto
//                return! wact |> WorldAction.createWorld
//            }


//type JobFileDto = {cat:string; value:string}
//module JobFileDto =
//    let toDto (jf:JobFile) =
//         match jf with
//         | JobFile.ReadWorld n -> {cat="ReadWorld"; value = Json.serialize n}
//         | JobFile.MakeWorld n -> {cat="MakeWorld"; value = Json.serialize n}


//    let fromDto (eDto:JobFileDto) =
//        if eDto.cat = "ReadWorld" then
//            result {
//                let! dto = eDto.value |> Json.deserialize<WorldDto>
//                return JobFile.ReadWorld dto
//            }
//        else if eDto.cat = "MakeWorld" then
//            result {
//                let! dto = eDto.value |> Json.deserialize<WorldActionDto>
//                return JobFile.MakeWorld dto
//            }
//        else sprintf "cat: %s for JobFileDto not found"
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
