namespace global


type enviro = 
    | Empty
    | ObjectMap of Map<string, string>


module Enviro =

    let toMap (e:enviro) =
        match e with
        | Empty -> "an Empty not an ObjectMap" |> Error
        | ObjectMap m -> m |> Ok


    let addKvpToEnviro (key:string) (dto) (e:enviro) =
         match e with
         | Empty -> (enviro.ObjectMap ([(key, dto|>Json.serialize)] 
                        |> Map.ofList)) |> Ok
         | ObjectMap m -> result {
                     let! mN = m |> ResultMap.add key (dto|>Json.serialize)
                     return enviro.ObjectMap mN
                 }

    let addDto<'T> (key:string) 
                   (dto:'T) 
                   (e:enviro) =
         let cereal = Json.serialize dto
         match e with
         | Empty -> (enviro.ObjectMap ([(key, cereal)] 
                        |> Map.ofList)) |> Ok
         | ObjectMap m -> result {
                     let! mN = m |> ResultMap.add key cereal
                     return enviro.ObjectMap mN
                 }


    let getDto<'T> (e:enviro) (key:string) =
         match e with
         | Empty -> "Empty not supported" |> Error
         | ObjectMap m -> result {
                     let! mN = m |> ResultMap.read key
                     return! Json.deserialize<'T>(mN)
                 }
