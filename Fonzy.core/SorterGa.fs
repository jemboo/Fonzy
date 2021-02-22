namespace global

type SorterGenome = 
    | Permutaions of List<TwoCyclePerm>
    | Switches of List<Switch>

type SorterPhenotype = 
    | Singleton of Sorter
    | Multiple of List<Sorter>

type SorterTestResults = 
    | Singleton of SwitchUses
    | Multiple of List<SwitchUses>

type SorterPhenotypeEval = 
    | Empty
    | B of int
