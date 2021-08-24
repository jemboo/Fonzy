namespace global

type SorterGenome = 
    | Permutaions of List<TwoCyclePerm>
    | DualPerm of List<TwoCyclePerm * TwoCyclePerm * int>
    | Switches of List<Switch>

type SorterPhenotype = 
    | Singleton of sorter
    | Multiple of List<sorter>

type SorterTestResults = 
    | Singleton of switchUses
    | Multiple of List<switchUses>

type SorterPhenotypeEval = 
    | Singleton of float
    | Multiple of List<float>
