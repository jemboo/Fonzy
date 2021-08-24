namespace global


type Enviro = 
    | Empty
    | ObjectMap of Map<string, string>


module Enviro =

    let toMap (e:Enviro) =
        match e with
        | Empty -> "an Empty not an ObjectMap" |> Error
        | ObjectMap m -> m |> Ok


    let addKvpToEnviro (key:string) (dto) (e:Enviro) =
         match e with
         | Empty -> (Enviro.ObjectMap ([(key, dto|>Json.serialize)] 
                        |> Map.ofList)) |> Ok
         | ObjectMap m -> result {
                     let! mN = m |> ResultMap.add key (dto|>Json.serialize)
                     return Enviro.ObjectMap mN
                 }

    let addDto<'T> (key:string) 
                   (dto:'T) 
                   (e:Enviro) =
         let cereal = Json.serialize dto
         match e with
         | Empty -> (Enviro.ObjectMap ([(key, cereal)] 
                        |> Map.ofList)) |> Ok
         | ObjectMap m -> result {
                     let! mN = m |> ResultMap.add key cereal
                     return Enviro.ObjectMap mN
                 }


    let getDto<'T> (e:Enviro) (key:string) =
         match e with
         | Empty -> "Empty not supported" |> Error
         | ObjectMap m -> result {
                     let! mN = m |> ResultMap.read key
                     return! Json.deserialize<'T>(mN)
                 }
