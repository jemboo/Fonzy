namespace global
open System


type CauseType =
    | Destroy
    | NoOp
    | Keyed of Guid * Map<string, string>

type Cause = {causeType:CauseType; op:Enviro -> Result<Enviro, string>}

module KeyedCauses = 
    let goo (id:Guid) (objectMap:Map<string,string>) =
        let yab = (string id)
        match yab with 
        | "dfd" -> yab
        | "aaa" -> yab
        | "gg" -> yab

    let ofKeyed (id:Guid) (objectMap:Map<string,string>) =
        {causeType= CauseType.Keyed (id, objectMap); op= fun e->e |> Ok }

module Cause = 
    let destroy = {causeType=CauseType.Destroy; op = fun e->Enviro.Empty |> Ok}
    let noOp = {causeType=CauseType.NoOp; op = fun e->e |> Ok}

    let fromCauseType (causeType:CauseType) = 
        match causeType with
            | Destroy -> destroy
            | NoOp -> noOp
            | Keyed (id, map) -> KeyedCauses.ofKeyed id map