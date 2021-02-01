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
    let fromMap map =
        Enviro.ObjectMap map