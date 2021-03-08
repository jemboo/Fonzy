namespace global
open System


module TestData = 
    let seed = 1234
    let iRando = Rando.fromRngGen (RngGen.createLcg seed)
    let degree = Degree.fromInt 8

    module ComboStructures =
        let permutation = Permutation.rotate degree 1


    module SorterParts =
        let switchCount = SwitchCount.fromInt 10
        let permSwitchDensity = 0.5
        let sorterLength = SorterLength.degreeToRecordStageCount degree
        let sorterCount = SorterCount.fromInt 50
        let makeRandomSorter() = 
                Sorter.createRandom degree sorterLength SwitchFrequency.max iRando

        let makeRandomTwoCycle = 
            TwoCyclePerm.makeRandomTwoCycle degree iRando permSwitchDensity
        let switchUseArray = Array.init (SwitchCount.value switchCount) 
                                        (fun _ -> iRando.NextPositiveInt)

        let switchList = Switch.switchMap 
                                      |> Seq.take (SwitchCount.value switchCount)
                                      |> Seq.toList


        let listOfSorters = List.init (SorterCount.value sorterCount)
                                      (fun _ -> makeRandomSorter())

        let sorterSet = SorterSet.fromSorters degree listOfSorters

        let rollupOfAllBinary = SortableSetRollup.allBinary degree
        let rollupOfAllSortedBinary = SortableSetRollup.fromSortableIntArrays 
                                        degree 
                                        (SortableIntArray.allSorted_0_1 degree)


    module SorterGa =

        let permutationsCount = 10
        let sorterCount = SorterCount.fromInt 10
        let switchUsesListLength = 5

        let twoCycleList = TwoCyclePerm.makeAllMonoCycles degree
                           |> Seq.toList

        let listOfSwitchUses = 
            List.init switchUsesListLength (fun _ -> 
                        SwitchUses.create SorterParts.switchCount 
                                          SorterParts.switchUseArray)
                      |> Result.sequence 
                      |> Result.ExtractOrThrow
                      
        let sorterList = 
            List.init (SorterCount.value sorterCount)
                      (fun _ -> SorterParts.makeRandomSorter())