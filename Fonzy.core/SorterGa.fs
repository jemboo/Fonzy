﻿namespace global

type SorterGenome = 
    | Permutaions of List<TwoCyclePerm>
    | DualPerm of List<TwoCyclePerm * TwoCyclePerm * int>
    | Switches of List<Switch>

type SorterPhenotype = 
    | Singleton of Sorter
    | Multiple of List<Sorter>

type SorterTestResults = 
    | Singleton of SwitchUses
    | Multiple of List<SwitchUses>

type SorterPhenotypeEval = 
    | Singleton of float
    | Multiple of List<float>
