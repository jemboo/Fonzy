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



//type EnvirosDto = {cat:string; value:string}
//module EnvirosDto =
//    let toDto (env:Enviro) =
//        match env with
//        | Enviro.B fv -> {cat="B"; value = Json.serialize fv}
//        | Enviro.FloatPoolEnviro n -> {cat="FloatPoolEnviro"; value = Json.serialize n}
//        | Enviro.SorterPoolEnviro n -> {cat="SorterPoolEnviro"; value = n |> SorterPoolEnviroDto.toDto |> Json.serialize}
//        | Enviro.Empty -> {cat="Empty"; value = Json.serialize None}

//    let fromDto (eDto:EnvirosDto) =
//        if eDto.cat = "Empty" then
//            result {
//                return Enviro.Empty
//            }
//        else if eDto.cat = "SorterPoolEnviro" then
//            result {
//                let! b = Json.deserialize<SorterPoolEnviroDto> eDto.value
//                let! q = b |> SorterPoolEnviroDto.fromDto
//                return Enviro.SorterPoolEnviro q
//            }
//        else if eDto.cat = "FloatPoolEnviro" then
//            result {
//                let! b = Json.deserialize<NumberPoolEnviro> eDto.value
//                return Enviro.FloatPoolEnviro b
//            }
//        else if eDto.cat = "B" then
//            result {
//                let! b = Json.deserialize<float> eDto.value
//                return Enviro.B b
//            }
//        else sprintf "cat: %s for EnviroDto not found"
//                      eDto.cat |> Error

