namespace global
open System

module rr = 
    let q = None

//type Job = 
//     | GetWorld of World
//     | MakeWorld of WorldAction

//module Job = 
//    let getId (job:Job) = 
//        match job with
//        | GetWorld w -> w.id
//        | MakeWorld wa -> wa.childId

//    let getParentId (job:Job) = 
//        match job with
//        | GetWorld w -> w.parentId
//        | MakeWorld wa -> Some wa.parentWorld.id

//    let getCause (job:Job) = 
//        match job with
//        | GetWorld w -> w.cause
//        | MakeWorld wa -> wa.cause







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

