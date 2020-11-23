namespace global
open System

type SorterPoolEnviroDto = {cat:string; value:string}
module SorterPoolEnviroDto =
    let toDto (env:SorterPoolEnviro) =
         match env with
         | SorterPoolEnviro.Bag n -> {cat="Bag"; value = Json.serialize n}
         | SorterPoolEnviro.Torus n -> {cat="Torus"; value = Json.serialize n}


    let fromDto (eDto:SorterPoolEnviroDto) =
        if eDto.cat = "Bag" then
            result {
                let! b = Json.deserialize<int> eDto.value
                return SorterPoolEnviro.Bag b
            }
        else if eDto.cat = "Torus" then
            result {
                let! b = Json.deserialize<int> eDto.value
                return SorterPoolEnviro.Torus b
            }
        else sprintf "cat: %s for SorterPoolEnviroDto not found"
                      eDto.cat |> Error


type EnviroDto = {cat:string; value:string}
module EnviroDto =
    let toDto (env:Enviro) =
        match env with
        | Enviro.B fv -> {cat="B"; value = Json.serialize fv}
        | Enviro.FloatPoolEnviro n -> {cat="FloatPoolEnviro"; value = Json.serialize n}
        | Enviro.SorterPoolEnviro n -> {cat="SorterPoolEnviro"; value = n |> SorterPoolEnviroDto.toDto |> Json.serialize}
        | Enviro.Empty -> {cat="Empty"; value = Json.serialize None}

    let fromDto (eDto:EnviroDto) =
        if eDto.cat = "Empty" then
            result {
                return Enviro.Empty
            }
        else if eDto.cat = "SorterPoolEnviro" then
            result {
                let! b = Json.deserialize<SorterPoolEnviroDto> eDto.value
                let! q = b |> SorterPoolEnviroDto.fromDto
                return Enviro.SorterPoolEnviro q
            }
        else if eDto.cat = "FloatPoolEnviro" then
            result {
                let! b = Json.deserialize<NumberPoolEnviro> eDto.value
                return Enviro.FloatPoolEnviro b
            }
        else if eDto.cat = "B" then
            result {
                let! b = Json.deserialize<float> eDto.value
                return Enviro.B b
            }
        else sprintf "cat: %s for EnviroDto not found"
                      eDto.cat |> Error


//type TestCauseTypeDto = {cat:string; value:string}
//module TestCauseTypeDto =
                      
//    let toDto (ct:FloatCauseType) =
//        match ct with
//        | FloatCauseType.AddFloat n -> {cat="AddFloat"; value = Json.serialize n}
//        | FloatCauseType.AddInt n -> {cat="AddInt"; value = Json.serialize n}
//        | FloatCauseType.CreateFloat n -> {cat="CreateFloat"; value = Json.serialize n}
//        | FloatCauseType.CreateInt n -> {cat="CreateInt"; value = Json.serialize n}
                      
//    let fromDto (eDto:TestCauseTypeDto) =
//        if eDto.cat = "AddFloat" then
//            result {
//                let! b = Json.deserialize<float> eDto.value
//                return TestCauseType.AddFloat b
//            }
//        else if eDto.cat = "AddInt" then
//            result {
//                let! b = Json.deserialize<int> eDto.value
//                return TestCauseType.AddInt b
//            }
//        else if eDto.cat = "CreateFloat" then
//            result {
//                let! b = Json.deserialize<float> eDto.value
//                return TestCauseType.CreateFloat b
//            }
//        else if eDto.cat = "CreateInt" then
//            result {
//                let! b = Json.deserialize<int> eDto.value
//                return TestCauseType.CreateInt b
//            }
//        else sprintf "cat: %s for TestCauseTypeDto not found"
//                    eDto.cat |> Error


//type CauseTypeDto = {cat:string; value:string}
//module CauseTypeDto =

//    let toDto (ct:CauseType) =
//        match ct with
//        | CauseType.Test tc -> {cat="Test"; value = tc |> TestCauseTypeDto.toDto |> Json.serialize}
//        | CauseType.Destroy -> {cat="Destroy"; value = Json.serialize None}
//        | CauseType.NoOp -> {cat="NoOp"; value = Json.serialize None}

//    let fromDto (eDto:CauseTypeDto) =
//        if eDto.cat = "Test" then
//            result {
//                let! dto = Json.deserialize<TestCauseTypeDto> eDto.value
//                let! tct = TestCauseTypeDto.fromDto dto
//                return CauseType.Test tct
//            }
//        else if eDto.cat = "Destroy" then
//            result {
//                return CauseType.Destroy
//            }
//        else if eDto.cat = "NoOp" then
//            result {
//                return CauseType.NoOp
//            }
//        else sprintf "cat: %s for CauseTypeDto not found"
//                      eDto.cat |> Error


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