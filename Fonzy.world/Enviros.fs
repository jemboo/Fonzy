namespace global
open System


type Enviro = 
    | Empty
    | ObjectMap of Map<string, string>
    | MergeSet of Map<Guid, Map<string, string>>


module Enviro =

    let toMap (e:Enviro) =
        match e with
        | Empty -> "an Empty not an ObjectMap" |> Error
        | ObjectMap m -> m |> Ok
        | MergeSet s -> "a MergeSet not an ObjectMap" |> Error


    let addKvpToEnviro (key:string) (dto) (e:Enviro) =
         match e with
         | Empty -> (Enviro.ObjectMap ([(key, dto|>Json.serialize)] 
                        |> Map.ofList)) |> Ok
         | ObjectMap m -> result {
                     let! mN = m |> ResultMap.add key (dto|>Json.serialize)
                     return Enviro.ObjectMap mN
                 }
         | MergeSet s -> failwith "not supported"


    let replaceKvpToEnviro (key:string) (dto) (e:Enviro) =
         match e with
         | Empty -> (Enviro.ObjectMap ([(key, dto|>Json.serialize)] 
                        |> Map.ofList)) |> Ok
         | ObjectMap m -> result {
                     let mN = m |> Map.add key (dto|>Json.serialize)
                     return Enviro.ObjectMap mN
                 }
         | MergeSet s -> failwith "not supported"