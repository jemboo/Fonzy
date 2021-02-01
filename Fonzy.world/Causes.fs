namespace global
open System


type CauseType =
    | Destroy
    | NoOp
    | Keyed of Guid * Map<string, string>

type Cause = {causeType:CauseType; op:Enviro -> Result<Enviro, string>}

module KeyedCauses = 
    
    //let addRandomInts (id:Guid) (objectMap:Map<string,string>) =
    //    let addEm (name:string) (ints:int[]) =
    //        fun e ->
    //        match e with
    //            | Empty -> Enviro.ObjectMap ([(name, ints |> Json.serialize)] |> Map.ofList ) |> Ok
    //            | ObjectMap m -> Enviro.ObjectMap (m |> Map.add name (ints |> Json.serialize)) |> Ok
    //    let r = objectMap.["a"]
    //    result {
    //        let r = objectMap.["a"]
        
    //    }

    //    { causeType = CauseType.Keyed (id, objectMap);
    //      op = addEm "nameo" [|1;2|] }


    let goo (id:Guid) (objectMap:Map<string,string>) =
        let idStr = (string id)
        match idStr with 
        | "dfd" -> idStr |> Ok
        | "aaa" -> idStr |> Ok
        | "gg" -> idStr |> Ok
        | _ -> sprintf "%s is not mapped" idStr |> Error

    let ofKeyed (id:Guid) (objectMap:Map<string,string>) =
        {causeType = CauseType.Keyed (id, objectMap); 
         op = fun e->e |> Ok }

module Cause = 
    let destroy = {causeType=CauseType.Destroy; op = fun e->Enviro.Empty |> Ok}
    let noOp = {causeType=CauseType.NoOp; op = fun e->e |> Ok}

    let fromCauseType (causeType:CauseType) = 
        match causeType with
            | Destroy -> destroy
            | NoOp -> noOp
            | Keyed (id, map) -> KeyedCauses.ofKeyed id map