namespace global

open System

type TestCauseType =
    | AddInt of int
    | AddFloat of float
    | CreateFloat of float
    | CreateInt of int

module TestCause = 
    let addFloat v = 
        fun e-> 
                match e with 
                 | Enviroment.B l -> Enviroment.B (l + v) |> Ok
                 | _ -> "wrong Enviroment for addFloat" |> Error

    let addInt v = 
         fun e-> 
                match e with 
                 | Enviroment.A l -> Enviroment.A (l + v) |> Ok
                 | _ -> "wrong Enviroment for addInt" |> Error


    let createFloat v = fun e -> Enviroment.B v |>Ok
    let createInt v =  fun e -> Enviroment.A v |> Ok

    let fromTestCauseType (causeType:TestCauseType) = 
        match causeType with
            | AddInt v -> addInt v
            | AddFloat v -> addFloat v
            | CreateFloat v -> createFloat v
            | CreateInt v -> createInt v


type CauseType =
    | Test of TestCauseType
    | Destroy
    | NoOp


type Cause = {causeType:CauseType; op:Enviroment -> Result<Enviroment, string>}

module Cause = 
    let destroy = {causeType=CauseType.Destroy; op = fun e->Enviroment.Empty |> Ok}
    let noOp = {causeType=CauseType.NoOp; op = fun e->e |> Ok}

    let fromCauseType (causeType:CauseType) = 
        match causeType with
            | Test tct -> {causeType=CauseType.Test tct; op = TestCause.fromTestCauseType tct}
            | Destroy -> {causeType=CauseType.Destroy; op = fun e->Enviroment.Empty |> Ok}
            | NoOp -> {causeType=CauseType.NoOp; op = fun e->e |> Ok}


type World = {id:Guid; parentId:Guid option; cause:Cause; enviroment:Enviroment}

module World = 
    let create (id:Guid) (parentId:Guid option) (cause:Cause) (enviroment:Enviroment) =
            {id=id; parentId=parentId; cause=cause; enviroment=enviroment}

    let createFromParent (id:Guid) (parentWorld:World) (cause:Cause) =
        result {
            let! newEnv = cause.op parentWorld.enviroment
            return {id=id; parentId= Some parentWorld.id; cause=cause; enviroment=newEnv}
        }
     

type WorldAction = {parentWorld:World; childId:Guid; cause:Cause;}
module WorldAction = 
    let create (parentWorld:World) (childId:Guid) (cause:Cause) =
            {parentWorld=parentWorld; childId=childId; cause=cause;}

    let createWorld (worldAction:WorldAction) =
        World.createFromParent worldAction.childId worldAction.parentWorld worldAction.cause


