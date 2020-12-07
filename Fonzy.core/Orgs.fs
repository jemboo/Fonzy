namespace global
open System

type Ancestry = 
        | NoAncestry
        | SingleParent of OrgId
        | SingleDistantParent of OrgId * GenerationNumber


type Genome = 
        | NoGeneome
        | Floater of float


type Phenotype = 
        | NoPhenotype
        | Floater of float


type PhenotypeEval = 
        | NoPhenotypeEval
        | Floater of float


type Org =
    {
        orgId:OrgId
        ancestry:Ancestry
        genome:Genome
        generation:GenerationNumber
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