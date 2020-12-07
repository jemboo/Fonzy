namespace global
open System

type CausesDto = {name:string}
module CausesDto = 
    let q = None



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
