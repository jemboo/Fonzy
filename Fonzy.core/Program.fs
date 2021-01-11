// Learn more about F# at http://fsharp.org

open System

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code








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
