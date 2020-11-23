namespace global
open System

type NumberGenome = 
    | Float of float
    | Other

type NumberPhenotype = 
    | Float of float
    | Other

type NumberTestResults = 
    | Float of float
    | Other

type NumberOrgType =
    | Regular
    | Other

type NumberOrg =
    {
        numberOrgId:NumberOrgId
        numberOrgType:NumberOrgType
        parentId:NumberOrgId option
        generation:GenerationNumber
        floatPhenotype:float
        phenotypeEval:float
    }

type NumberOrgs =
    {
        id:Guid;
        numberOrgs:Map<NumberOrgId, NumberOrg>;
    }

type NumberOrgsWithGridLocs =
    {
        id:Guid;
        numberOrgs:Map<NumberOrgId, NumberOrg>;
        poolOfGridLocations: PoolOfGridLocations
    }

type NumberPoolEnviro =
    | Bag of NumberOrgs
    | Grid of NumberOrgsWithGridLocs


module NumberPoolEnviro = 
    let createBag (kvps: (NumberOrgId*NumberOrg)[]) =
        //NumberPoolEnviro.Bag (kvps |> Map.ofArray)
        None

    let createGrid (kvps: (NumberOrgId*NumberOrg)[]) =
        //NumberPoolEnviro.Grid (kvps |> Map.ofArray)
        None 

    let createBag2 (orgs: (NumberOrg)[]) =
        //orgs |> Array.map(fun o-> (o.numberOrgId, o))
        //     |> Map.ofArray |> NumberPoolEnviro.Bag
        None

    let createGrid2 (orgs: NumberOrg[]) 
                    (gridLocations:GridLocation[]) =
        //let orgMap = orgs |> Array.map(fun o-> (o.numberOrgId, o))
        //                  |> Map.ofArray |> NumberPoolEnviro.Grid
        None 






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

