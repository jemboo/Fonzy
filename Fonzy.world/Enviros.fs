namespace global
open System


type ObjectMapEnviro =
    {
        id:Guid;
        orgs:Orgs;
        objectMap: Map<string, string>
    }


type Enviro = 
    | Empty
    | ObjectMap of Map<string, string>

module Enviro =
    let toMap (e:Enviro) =
        match e with
        | Empty -> Map.empty
        | ObjectMap m -> m

    let addKvpToEnviro (key:string) (dto) (e:Enviro) =
         match e with
         | Empty -> (Enviro.ObjectMap ([(key, dto|>Json.serialize)] |> Map.ofList)) |> Ok
         | ObjectMap m -> result {
                     let! mN = m |> ResultMap.add key (dto|>Json.serialize)
                     return Enviro.ObjectMap mN
                 }

    let replaceKvpToEnviro (key:string) (dto) (e:Enviro) =
         match e with
         | Empty -> (Enviro.ObjectMap ([(key, dto|>Json.serialize)] |> Map.ofList)) |> Ok
         | ObjectMap m -> result {
                     let mN = m |> Map.add key (dto|>Json.serialize)
                     return Enviro.ObjectMap mN
                 }