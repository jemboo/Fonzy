namespace global


type Enviro = 
    | Empty
    | ObjectMap of Map<string, string>
    | RootDto of RootDto


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


    let addRootDtoToEnviro<'T> (e:Enviro) (key:string) (dto:'T) 
                               (metadata:Map<string,string>)  =
         let cereal = RootDto.toJson<'T> dto metadata
         match e with
         | Empty -> (Enviro.ObjectMap ([(key, cereal)] 
                        |> Map.ofList)) |> Ok
         | ObjectMap m -> result {
                     let! mN = m |> ResultMap.add key cereal
                     return Enviro.ObjectMap mN
                 }


    let addDto<'T> (e:Enviro) 
                   (key:string) 
                   (dto:'T) =
         let cereal = Json.serialize dto
         match e with
         | Empty -> (Enviro.ObjectMap ([(key, cereal)] 
                        |> Map.ofList)) |> Ok
         | ObjectMap m -> result {
                     let! mN = m |> ResultMap.add key cereal
                     return Enviro.ObjectMap mN
                 }



    let getDtoAndMetaFromEnviro<'T> (e:Enviro) 
                                    (key:string) =
         match e with
         | Empty -> "Empty not supported" |> Error
         | ObjectMap m -> result {
                     let! mN = m |> ResultMap.read key
                     let! ofT, meta = mN |> RootDto.extractFromJson<'T>
                     return ofT, meta
                 }


    let getDto<'T> (e:Enviro) (key:string) =
         match e with
         | Empty -> "Empty not supported" |> Error
         | ObjectMap m -> result {
                     let! mN = m |> ResultMap.read key
                     return! Json.deserialize<'T>(mN)
                 }


    let replaceKvpToEnviro (e:Enviro) (key:string) (dto) 
                           (metadata:Map<string,string>)  =
         let cereal = RootDto.toJson dto metadata
         match e with
         | Empty -> (Enviro.ObjectMap ([(key, cereal)] 
                        |> Map.ofList)) |> Ok
         | ObjectMap m -> result {
                     let mN = m |> Map.add key cereal
                     return Enviro.ObjectMap mN
                 }