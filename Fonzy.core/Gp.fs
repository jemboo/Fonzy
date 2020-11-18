namespace global

type Genome = 
    | A of int
    | B of int

type Phenotype = 
    | A of int
    | B of int

type TestResults = 
    | A of int
    | B of int

type PhenotypeEval = private PhenotypeEval of float

type SorterPoolEnviro = 
    | Bag of int
    | Torus of int


type OrgType =
    | A of int
    | B of int


type Org = 
    {
        id:OrgId
        orgType:OrgType
        enviroment:SorterPoolEnviro
        parentId:OrgId option
        generation:GenerationNumber;
        genome:Genome;
        phenotype:Phenotype;
        testResults:TestResults;
        phenotypeEval:PhenotypeEval
    }

module SorterPoolEnviro =

    let getOrg (e:SorterPoolEnviro) = 
        match e with
        | SorterPoolEnviro.Bag i -> None
        | SorterPoolEnviro.Torus i -> None


