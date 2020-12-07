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