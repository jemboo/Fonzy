namespace global

type sorterGenome = 
    | Permutaions of List<twoCyclePerm>
    | DualPerm of List<twoCyclePerm * twoCyclePerm * int>
    | Switches of List<Switch>

type sorterPhenotype = 
    | Singleton of sorter
    | Multiple of List<sorter>

type sorterTestResults = 
    | Singleton of switchUses
    | Multiple of List<switchUses>

type sorterPhenotypeEval = 
    | Singleton of float
    | Multiple of List<float>
