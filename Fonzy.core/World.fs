namespace global

open System

type CauseType =
    | AddInt of int
    | AddFloat of float
    | CreateFloat of float
    | CreateInt of int
    | Destroy
    | NoOp


type Cause = {causeType:CauseType; op:Enviroment -> Result<Enviroment, string>}

module Cause = 
    let addFloat v = 
        {
            causeType=CauseType.AddFloat v; 
            op = fun e-> 
                match e with 
                 | Enviroment.B l -> Enviroment.B (l + v) |> Ok
                 | _ -> "wrong Enviroment for addFloat" |> Error
         }

    let addInt v = 
        {
            causeType=CauseType.AddInt v; 
            op = fun e-> 
                match e with 
                 | Enviroment.A l -> Enviroment.A (l + v) |> Ok
                 | _ -> "wrong Enviroment for addInt" |> Error
         }

    let createFloat v = {causeType=CauseType.CreateFloat v; op = fun e->Enviroment.B v |>Ok}
    let createInt v = {causeType=CauseType.CreateInt v; op = fun e->Enviroment.A v |> Ok}
    let destroy = {causeType=CauseType.Destroy; op = fun e->Enviroment.Empty |> Ok}
    let noOp = {causeType=CauseType.NoOp; op = fun e->e |> Ok}

    let fromCauseType (causeType:CauseType) = 
        match causeType with
            | AddInt v -> addInt v
            | AddFloat v -> addFloat v
            | CreateFloat v -> createFloat v
            | CreateInt v -> createInt v
            | Destroy -> destroy
            | NoOp -> noOp




type World = {id:Guid; parentId:Guid option; cause:Cause; enviroment:Enviroment}

module World = 
    let create (id:Guid) (parentId:Guid option) (cause:Cause) (enviroment:Enviroment) =
            {id=id; parentId=parentId; cause=cause; enviroment=enviroment}

    let createFromParent (id:Guid) (parentWorld:World) (cause:Cause) =
        result {
            let! newEnv = cause.op parentWorld.enviroment
            return {id=id; parentId= Some parentWorld.id; cause=cause; enviroment=newEnv}
        }
     
