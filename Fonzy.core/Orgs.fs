namespace global
open System

type Ancestry = 
        | NoAncestry
        | SingleParent of OrgId
        | SingleDistantParent of OrgId * GenerationNumber



type Genome = 
        | NoGenome
        | Floater of float


type Phenotype = 
        | NoPhenotype
        | FloaterP of float


type PhenotypeEval = 
        | NoPhenotypeEval
        | FloaterE of float


type Org =
    {
        orgId:OrgId
        ancestry:Ancestry
        generation:GenerationNumber
        genome:Genome
        phenotype:Phenotype
        phenotypeEval:PhenotypeEval
    }


module Org = 
    let create (orgId:OrgId) (ancestry:Ancestry)
               (genome:Genome) (generation:GenerationNumber) 
               (phenotype:Phenotype) (phenotypeEval:PhenotypeEval) =
        {
            Org.orgId = orgId;
            Org.ancestry = ancestry;
            Org.genome = genome;
            Org.generation = generation;
            Org.phenotype = phenotype;
            Org.phenotypeEval = phenotypeEval;
        }

    let budWithNewGenome (org:Org) (newId:OrgId) (newGenome:Genome) =
        {
            Org.orgId = newId;
            Org.ancestry = Ancestry.SingleParent org.orgId;
            Org.genome = newGenome;
            Org.generation = org.generation |> GenerationNumber.increment;
            Org.phenotype = Phenotype.NoPhenotype;
            Org.phenotypeEval = PhenotypeEval.NoPhenotypeEval;
        }

    let assignPhenotype (org:Org) (newPhenotype:Phenotype) =
        {
            Org.orgId = org.orgId;
            Org.ancestry = Ancestry.SingleParent org.orgId;
            Org.genome = org.genome;
            Org.generation = org.generation;
            Org.phenotype = newPhenotype;
            Org.phenotypeEval = PhenotypeEval.NoPhenotypeEval;
        }

    let assignPhenotypeEval (org:Org) (newPhenotypeEval:PhenotypeEval) =
        {
            Org.orgId = org.orgId;
            Org.ancestry = Ancestry.SingleParent org.orgId;
            Org.genome = org.genome;
            Org.generation = org.generation;
            Org.phenotype = org.phenotype;
            Org.phenotypeEval = newPhenotypeEval;
        }


type Orgs =
    {
        id:Guid;
        orgMap:Map<OrgId, Org>;
    }

module Orgs =
    let create (id:Guid) (orgs: Org[]) =
        {
            Orgs.id = id;
            orgMap = orgs |> Array.map(fun o-> (o.orgId, o))
                          |> Map.ofArray
        }

    let getMembers (orgs:Orgs) =
        orgs.orgMap |> Map.toSeq
                    |> Seq.map(snd)



//type OrgsWithGridLocs =
//    {
//        id:Guid;
//        orgMap:Map<OrgId, Org>;
//        poolOfGridLocations: PoolOfGridLocations
//    }

//module OrgsWithGridLocs =
//    let create (id:Guid) (orgs: Org[]) 
//               (poolOfGridLocations:PoolOfGridLocations) =
//        {
//            OrgsWithGridLocs.id = id;
//            orgMap = orgs |> Array.map(fun o-> (o.orgId, o))
//                              |> Map.ofArray;
//            poolOfGridLocations = poolOfGridLocations;
//        }


//type OrgPoolEnviro =
//    | Bag of Orgs
//    | Grid of OrgsWithGridLocs


//module OrgPoolEnviro = 
//    let createBag (numberOrgs: Orgs) =
//        OrgPoolEnviro.Bag numberOrgs

//    let createGrid (numberOrgsWithGridLocs: OrgsWithGridLocs) =
//        OrgPoolEnviro.Grid numberOrgsWithGridLocs





module Ancestry = 
    let makeSingleParent (parentOrg:Org) =
        match parentOrg.ancestry  with
        | NoAncestry -> Ancestry.SingleParent parentOrg.orgId
        | SingleParent p -> parentOrg.ancestry
        | SingleDistantParent (p,g) ->  Ancestry.SingleParent parentOrg.orgId

    let makeSingleDistantParent (parentOrg:Org) =
        match parentOrg.ancestry  with
        | NoAncestry -> Ancestry.SingleDistantParent (parentOrg.orgId, parentOrg.generation)
        | SingleParent p -> Ancestry.SingleDistantParent (parentOrg.orgId, parentOrg.generation)
        | SingleDistantParent (p,g) -> parentOrg.ancestry
        

module Genome =

    let getFloatValue (genome:Genome) =
        match genome with
        | Floater v -> v |> Ok
        | _ -> Error "wrong genome type for Genome.getFloatValue"

//type Genome = 
//        | NoGenome
//        | Floater of float


module Phenotype = 
    let passThroughEval (org:Org) =
        match org.genome with
        | Floater f -> Phenotype.FloaterP f |> Ok
        | _ -> Error "Invalid genome in Phenotype.passThroughEval"


module PhenotypeEval = 
    let passThroughEval (org:Org) =
        match org.phenotype with
        | FloaterP f -> PhenotypeEval.FloaterE f|> Ok
        | _ -> Error "Invalid genome in PhenotypeEval.passThroughEval"