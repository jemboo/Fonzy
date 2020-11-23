namespace global

open System

type Org =
    | FloatOrgOld of NumberOrg
    | SorterOrg of SorterOrg

module Org = 
    let isFloatOrg (org:Org) =
        match org with
        | FloatOrgOld f -> true
        | _ -> false

    let isSorterOrg (org:Org) =
        match org with
        | SorterOrg s -> true
        | _ -> false

//type SorterPoolState =
//    | Initiated
//    | Measured
//    | Evaluated
//    | Selected

//module SorterPoolState =
//    let IsCompatable (sorterPoolState:SorterPoolState) 
//                     (poolMemberState:PoolMemberState) =
//        match sorterPoolState with
//        | Initiated -> (poolMemberState = PoolMemberState.Root) || 
//                        (poolMemberState = PoolMemberState.Initiate) || 
//                        (poolMemberState = PoolMemberState.Legacy)
//        | Measured -> (poolMemberState = PoolMemberState.Measured)
//        | Evaluated -> (poolMemberState = PoolMemberState.Evaluated)
//        | Selected -> (poolMemberState = PoolMemberState.Evaluated)


//type SorterPool =
//  {
//        id: EnviroId;
//        degree:Degree;
//        sorterPoolMembers: SorterPoolMember list;
//        sorterPoolState: SorterPoolState;
//        generation:GenerationNumber
//  }

