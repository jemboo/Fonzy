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
                 | Enviro.B l -> Enviro.B (l + v) |> Ok
                 | _ -> "wrong Enviro for addFloat" |> Error

    let addInt v = 
         fun e-> 
                match e with 
                 | Enviro.A l -> Enviro.A (l + v) |> Ok
                 | _ -> "wrong Enviro for addInt" |> Error

    let createFloat v = fun e -> Enviro.B v |>Ok
    let createInt v =  fun e -> Enviro.A v |> Ok

    let fromTestCauseType (causeType:TestCauseType) = 
        match causeType with
            | AddInt v -> addInt v
            | AddFloat v -> addFloat v
            | CreateFloat v -> createFloat v
            | CreateInt v -> createInt v



type SorterPoolCauseType =
    | AddInt of int
    | AddFloat of float
    | CreateFloat of float
    | CreateInt of int

module SorterPoolCauseType = 
    let addFloat v = 
        fun e-> 
                match e with 
                 | Enviro.B l -> Enviro.B (l + v) |> Ok
                 | _ -> "wrong Enviro for addFloat" |> Error

    let addInt v = 
         fun e-> 
                match e with 
                 | Enviro.A l -> Enviro.A (l + v) |> Ok
                 | _ -> "wrong Enviro for addInt" |> Error

    let createFloat v = fun e -> Enviro.B v |>Ok
    let createInt v =  fun e -> Enviro.A v |> Ok

    let fromTestCauseType (causeType:SorterPoolCauseType) = 
        match causeType with
            | SorterPoolCauseType.AddInt v -> addInt v
            | SorterPoolCauseType.AddFloat v -> addFloat v
            | SorterPoolCauseType.CreateFloat v -> createFloat v
            | SorterPoolCauseType.CreateInt v -> createInt v



type CauseType =
    | Test of TestCauseType
    | Destroy
    | NoOp

type Cause = {causeType:CauseType; op:Enviro -> Result<Enviro, string>}
module Cause = 
    let destroy = {causeType=CauseType.Destroy; op = fun e->Enviro.Empty |> Ok}
    let noOp = {causeType=CauseType.NoOp; op = fun e->e |> Ok}
    let testCause tct = {causeType=CauseType.Test tct; op = TestCause.fromTestCauseType tct}

    let fromCauseType (causeType:CauseType) = 
        match causeType with
            | Test tct -> testCause tct
            | Destroy -> destroy
            | NoOp -> noOp
