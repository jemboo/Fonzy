namespace global
open System

//type FloatCauseType =
//    | AddInt of int
//    | AddFloat of float
//    | CreateFloat of float
//    | CreateInt of int

//module FloatCause = 
//    let addFloat v = 
//        fun e-> 
//                match e with 
//                 | Enviro.B l -> Enviro.B (l + v) |> Ok
//                 | _ -> "wrong Enviro for addFloat" |> Error

//    let createFloat v = fun e -> FloatPoolEnviro v |>Ok
//    let createInt v =  fun e -> FloatPoolEnviro v |> Ok

//    let fromFloatCauseType (causeType:FloatCauseType) = 
//        match causeType with
//            | AddFloat v -> addFloat v
//            | CreateFloat v -> createFloat v
//            | CreateInt v -> createInt v



type SorterPoolCauseType =
    | AddInt
    | AddFloat
    | CreateFloat
    | CreateInt

module SorterPoolCauseType = 
    let addFloat  = 
        fun e-> 
                match e with 
                 | Enviro.B l -> Enviro.B (l) |> Ok
                 | _ -> "wrong Enviro for addFloat" |> Error

    let addInt  = 
         fun e-> 
                match e with 
                 | Enviro.FloatPoolEnviro _  -> "wrong Enviro for addInt" |> Error
                 | _ -> "wrong Enviro for addInt" |> Error

    let createFloat  = fun e -> Enviro.B 2.0 |>Ok
    let createInt  =  fun e -> Enviro.B 2.0 |> Ok

    let fromTestCauseType (causeType:SorterPoolCauseType) = 
        match causeType with
            | SorterPoolCauseType.AddInt  -> addInt 
            | SorterPoolCauseType.AddFloat  -> addFloat
            | SorterPoolCauseType.CreateFloat  -> createFloat 
            | SorterPoolCauseType.CreateInt  -> createInt 


//type CauseType =
//    | FloatCause of FloatCause
//    | Destroy
//    | NoOp

//type Cause = {causeType:CauseType; op:Enviro -> Result<Enviro, string>}
//module Cause = 
//    let destroy = {causeType=CauseType.Destroy; op = fun e->Enviro.Empty |> Ok}
//    let noOp = {causeType=CauseType.NoOp; op = fun e->e |> Ok}
//    let testCause tct = {causeType=CauseType.Test tct; op = TestCause.fromTestCauseType tct}

//    let fromCauseType (causeType:CauseType) = 
//        match causeType with
//            | Test tct -> testCause tct
//            | Destroy -> destroy
            //| NoOp -> noOp
