namespace global

type SorterGenome = 
    | A of int
    | B of int

type SorterPhenotype = 
    | A of int
    | B of int

type SorterTestResults = 
    | A of int
    | B of int

type SorterPhenotypeEval = private PhenotypeEval of float

type SorterPoolEnviro = 
    | Bag of int
    | Torus of int


type SorterOrgType =
    | A of int
    | B of int


type SorterOrg = 
    {
        id:SorterOrgId
        sorterOrgType:SorterOrgType
        enviroment:SorterPoolEnviro
        parentId:SorterOrgId option
        generation:GenerationNumber
        sorterGenome:SorterGenome
        sorterPhenotype:SorterPhenotype
        testResults:SorterTestResults
        phenotypeEval:SorterPhenotypeEval
    }

module SorterPoolEnviro =

    let getOrg (e:SorterPoolEnviro) = 
        match e with
        | SorterPoolEnviro.Bag i -> None
        | SorterPoolEnviro.Torus i -> None


