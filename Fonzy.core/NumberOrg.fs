namespace global
open System

module NumberOrg =

    let makeFloat (id:Guid) (floatVal:float) =
        result {
            let! orgId = OrgId.create id
            return {
                Org.orgId = orgId
                ancestry=Ancestry.NoAncestry
                generation=GenerationNumber.fromInt 0
                genome=Genome.Floater floatVal
                phenotype=Phenotype.NoPhenotype
                phenotypeEval=PhenotypeEval.NoPhenotypeEval
            }
        }

    let makeSuccessor (org:Org) (newId:Guid) (newVal:float) 
                      (ancestorer: Org->Ancestry) =
        result {
            let! newId = OrgId.create newId
            let nextGen = GenerationNumber.increment org.generation
            return {
                Org.orgId = newId
                ancestry = ancestorer org
                generation = nextGen
                genome = Genome.Floater newVal
                phenotype = Phenotype.NoPhenotype
                phenotypeEval = PhenotypeEval.NoPhenotypeEval
            }
        }
        

    let passThroughPhenotype (org:Org) =
            result {
                let! phenotype = Phenotype.passThroughEval org
                return {
                    Org.orgId = org.orgId
                    ancestry= org.ancestry
                    generation=org.generation
                    genome=org.genome
                    phenotype=phenotype
                    phenotypeEval=PhenotypeEval.NoPhenotypeEval
                }
            }

    let passThroughPhenotypeEval (org:Org) =
            result {
                let! phenotypeEval = PhenotypeEval.passThroughEval org
                return {
                    Org.orgId = org.orgId
                    ancestry= org.ancestry
                    generation=org.generation
                    genome=org.genome
                    phenotype=org.phenotype
                    phenotypeEval=phenotypeEval
                }
            }

    let genomeDelta (org:Org) (newId:Guid) (delta:float) =
            result {
                let! oldGenome =  org.genome |> Genome.getFloatValue 
                let newGenome = oldGenome + delta
                return! makeSuccessor org newId newGenome Ancestry.makeSingleParent
            }

    let genomeRandomDrift (org:Org) (rando:IRando) =
            result {
                let newGenome = (rando.NextFloat *  2.0)  - 1.0
                let newId = Rando.NextGuid rando None 
                return! makeSuccessor org newId newGenome
            }



module NumberOrgs = 

    let makeFloats (id:Guid) (memberIds:seq<Guid>) (floatVals:seq<float>) 
                   (bagCount:int) =
        result {

            let! dds = Seq.zip memberIds floatVals |> Seq.take bagCount
                      |> Seq.map(fun (id, v) -> NumberOrg.makeFloat id v)
                      |> Seq.toList
                      |> Result.sequence

            return Orgs.create id (dds |> List.toArray)
        }

    let makeBagOfZeroes (id:Guid) (memberIds:seq<Guid>)
                        (bagCount:int) =
        makeFloats id memberIds (Seq.replicate bagCount 0.0) bagCount

    let genomeRandomDrift (orgs:Orgs) (newOrgsId:Guid) (randy:IRando) = 
        let getDelta = (randy.NextFloat *  2.0)  - 1.0
        let getId = Rando.NextGuid randy None
        result {
            let! newOrgs = (Orgs.getMembers orgs) 
                          |> Seq.map(fun o ->  NumberOrg.genomeDelta o getId getDelta)
                          |> Seq.toList
                          |> Result.sequence

            return Orgs.create newOrgsId (newOrgs |> List.toArray)
        }


//type NumberOrgId = private NumberOrgId of Guid
//module NumberOrgId =
//    let value (NumberOrgId v) = v
//    let create id = Ok (NumberOrgId id)
//    let fromGuid (id:Guid) = create id |> Result.ExtractOrThrow

//type NumberGenome = 
//    | Float of float
//    | Other

//type NumberPhenotype = 
//    | Float of float
//    | Other

//type NumberTestResults = 
//    | Float of float
//    | Other

//type NumberOrgType =
//    | Regular
//    | Other

//type NumberOrg =
//    {
//        numberOrgId:NumberOrgId
//        numberOrgType:NumberOrgType
//        parentId:NumberOrgId option
//        generation:GenerationNumber
//        floatPhenotype:float
//        phenotypeEval:float
//    }

//type NumberOrgs =
//    {
//        id:Guid;
//        orgMap:Map<NumberOrgId, NumberOrg>;
//    }

//module NumberOrgs =
//    let create (id:Guid) (orgs: NumberOrg[]) =
//        {
//            NumberOrgs.id = id;
//            orgMap = orgs |> Array.map(fun o-> (o.numberOrgId, o))
//                              |> Map.ofArray
//        }

//type NumberOrgsWithGridLocs =
//    {
//        id:Guid;
//        orgMap:Map<NumberOrgId, NumberOrg>;
//        poolOfGridLocations: PoolOfGridLocations
//    }

//module NumberOrgsWithGridLocs =
//    let create (id:Guid) (orgs: NumberOrg[]) 
//               (poolOfGridLocations:PoolOfGridLocations) =
//        {
//            NumberOrgsWithGridLocs.id = id;
//            orgMap = orgs |> Array.map(fun o-> (o.numberOrgId, o))
//                              |> Map.ofArray;
//            poolOfGridLocations = poolOfGridLocations;
//        }


//type NumberPoolEnviro =
//    | Bag of NumberOrgs
//    | Grid of NumberOrgsWithGridLocs


//module NumberPoolEnviro = 
//    let createBag (numberOrgs: NumberOrgs) =
//        NumberPoolEnviro.Bag numberOrgs

//    let createGrid (numberOrgsWithGridLocs: NumberOrgsWithGridLocs) =
//        NumberPoolEnviro.Grid numberOrgsWithGridLocs