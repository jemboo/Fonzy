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

type Enviroment = 
    | Empty
    | A of int
    | B of float

type OrgType =
    | A of int
    | B of int


type Org = 
    {
        id:OrgId
        orgType:OrgType
        enviroment:Enviroment
        parentId:OrgId option
        generation:GenerationNumber;
        genome:Genome;
        phenotype:Phenotype;
        testResults:TestResults;
        phenotypeEval:PhenotypeEval
    }

module Enviroment =

    let getOrg (e:Enviroment) = 
        match e with
        | Enviroment.A i -> None
        | Enviroment.B i -> None


