namespace global
open System

type NumberOrgId = private NumberOrgId of Guid
module NumberOrgId =
    let value (NumberOrgId v) = v
    let create id = Ok (NumberOrgId id)
    let fromGuid (id:Guid) = create id |> Result.ExtractOrThrow

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
        orgMap:Map<NumberOrgId, NumberOrg>;
    }

module NumberOrgs =
    let create (id:Guid) (orgs: NumberOrg[]) =
        {
            NumberOrgs.id = id;
            orgMap = orgs |> Array.map(fun o-> (o.numberOrgId, o))
                              |> Map.ofArray
        }

type NumberOrgsWithGridLocs =
    {
        id:Guid;
        orgMap:Map<NumberOrgId, NumberOrg>;
        poolOfGridLocations: PoolOfGridLocations
    }

module NumberOrgsWithGridLocs =
    let create (id:Guid) (orgs: NumberOrg[]) 
               (poolOfGridLocations:PoolOfGridLocations) =
        {
            NumberOrgsWithGridLocs.id = id;
            orgMap = orgs |> Array.map(fun o-> (o.numberOrgId, o))
                              |> Map.ofArray;
            poolOfGridLocations = poolOfGridLocations;
        }


type NumberPoolEnviro =
    | Bag of NumberOrgs
    | Grid of NumberOrgsWithGridLocs


module NumberPoolEnviro = 
    let createBag (numberOrgs: NumberOrgs) =
        NumberPoolEnviro.Bag numberOrgs

    let createGrid (numberOrgsWithGridLocs: NumberOrgsWithGridLocs) =
        NumberPoolEnviro.Grid numberOrgsWithGridLocs