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

type SorterPoolEnvironment = 
    | Bag of int
    | Torus of int

type Enviroment = 
    | Empty
    | S of SorterPoolEnvironment
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
        | Enviroment.S s -> None


