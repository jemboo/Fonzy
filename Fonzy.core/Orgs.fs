namespace global
open System

type Ancestry = 
        | NoAncestry
        | SingleParent of OrgId
        | SingleDistantParent of OrgId * GenerationNumber


type Genome = 
        | NoGenome
        | Sorter of SorterGenome


type Phenotype = 
        | NoPhenotype
        | Sorter of SorterPhenotype


type OrgPerformance =
    | NoPerformance
    | Sorter of SorterTestResults


type PhenotypeEval = 
        | NoPhenotypeEval
        | Sorter of SorterPhenotypeEval


type Org =
    {
        orgId:OrgId
        ancestry:Ancestry
        generation:GenerationNumber
        genome:Genome
        phenotype:Phenotype
        orgPerformance:OrgPerformance
        phenotypeEval:PhenotypeEval
    }


module Org = 
    let create (orgId:OrgId) (ancestry:Ancestry)
               (genome:Genome) (generation:GenerationNumber) 
               (phenotype:Phenotype) (orgPerformance:OrgPerformance) 
               (phenotypeEval:PhenotypeEval) =
        {
            Org.orgId = orgId;
            Org.ancestry = ancestry;
            Org.genome = genome;
            Org.generation = generation;
            Org.phenotype = phenotype;
            Org.orgPerformance = orgPerformance;
            Org.phenotypeEval = phenotypeEval;
        }

    let budWithNewGenome (org:Org) (newId:OrgId) (newGenome:Genome) =
        {
            Org.orgId = newId;
            Org.ancestry = Ancestry.SingleParent org.orgId;
            Org.genome = newGenome;
            Org.generation = org.generation |> GenerationNumber.increment;
            Org.phenotype = Phenotype.NoPhenotype;
            Org.orgPerformance = OrgPerformance.NoPerformance;
            Org.phenotypeEval = PhenotypeEval.NoPhenotypeEval;
        }

    let assignPhenotype (org:Org) (newPhenotype:Phenotype) =
        {
            Org.orgId = org.orgId;
            Org.ancestry = Ancestry.SingleParent org.orgId;
            Org.genome = org.genome;
            Org.generation = org.generation;
            Org.phenotype = newPhenotype;
            Org.orgPerformance = OrgPerformance.NoPerformance;
            Org.phenotypeEval = PhenotypeEval.NoPhenotypeEval;
        }

    let assignOrgPerformace (org:Org) (newPhenotypeEval:PhenotypeEval) =
        {
            Org.orgId = org.orgId;
            Org.ancestry = Ancestry.SingleParent org.orgId;
            Org.genome = org.genome;
            Org.generation = org.generation;
            Org.phenotype = org.phenotype;
            Org.orgPerformance = org.orgPerformance;
            Org.phenotypeEval = newPhenotypeEval;
        }

    let assignPhenotypeEval (org:Org) (newPhenotypeEval:PhenotypeEval) =
        {
            Org.orgId = org.orgId;
            Org.ancestry = Ancestry.SingleParent org.orgId;
            Org.genome = org.genome;
            Org.generation = org.generation;
            Org.phenotype = org.phenotype;
            Org.orgPerformance = org.orgPerformance;
            Org.phenotypeEval = newPhenotypeEval;
        }


type Orgs =
    {
        id:OrgsId;
        orgMap:Map<OrgId, Org>;
        mapOfOrgAttributeMaps: Map<string, MapOfOrgAttributes<string>> option
    }


module Orgs =
    let create (id:Guid) (orgs: Org[]) 
               (mapOfOrgAttributeMaps: Map<string, MapOfOrgAttributes<string>> option) =
        {
            Orgs.id = OrgsId.fromGuid id;
            orgMap = orgs |> Array.map(fun o-> (o.orgId, o))
                          |> Map.ofArray
            mapOfOrgAttributeMaps = mapOfOrgAttributeMaps
        }

    let getMembers (orgs:Orgs) =
        orgs.orgMap |> Map.toSeq
                    |> Seq.map(snd)


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
        

//module Genome =

//    let getFloatValue (genome:Genome) =
//        match genome with
//        | Floater v -> v |> Ok
//        | _ -> Error "wrong genome type for Genome.getFloatValue"


//module Phenotype = 
//    let passThroughEval (org:Org) =
//        match org.genome with
//        | PhenotypeEval f -> Phenotype.FloaterP f |> Ok
//        | _ -> Error "Invalid genome in Phenotype.passThroughEval"


//module PhenotypeEval = 
//    let passThroughEval (org:Org) =
//        match org.phenotype with
//        | FloaterP f -> PhenotypeEval.FloaterE f|> Ok
//        | _ -> Error "Invalid genome in PhenotypeEval.passThroughEval"