namespace global

open System




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
//        id: EnviromentId;
//        degree:Degree;
//        sorterPoolMembers: SorterPoolMember list;
//        sorterPoolState: SorterPoolState;
//        generation:GenerationNumber
//  }

