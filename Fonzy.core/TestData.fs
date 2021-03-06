namespace global
open System


module TestData = 
    let seed = 1234
    let iRando = Rando.fromRngGen (RngGen.createLcg seed)
    let degree = Degree.fromInt 8

    module ComboStructures =
        let permutation = Permutation.createRandom degree iRando
        let yab = None

    module SorterParts =
        let switchCount = SwitchCount.fromInt 10
        let permSwitchDensity = 0.5
        let sorterLength = SorterLength.degreeToRecordStageCount degree
        let sorterCount = SorterCount.fromInt 50
        let makeRandomSorter() = 
                Sorter.createRandom degree sorterLength SwitchFrequency.max iRando
        let randomSwitches = Switch.randomSwitchesOfDegree degree iRando
        let makeRandomTwoCycle = 
            TwoCyclePerm.makeRandomTwoCycle degree iRando permSwitchDensity
        let switchUseArray = Array.init (SwitchCount.value switchCount) 
                                        (fun _ -> iRando.NextPositiveInt)
        let switchList = randomSwitches
                                |> Seq.take (SwitchCount.value switchCount)
                                |> Seq.toList

        let listOfSorters = List.init (SorterCount.value sorterCount)
                                      (fun _ -> makeRandomSorter())

        let sorterSet = SorterSet.fromSorters degree listOfSorters


    module SorterGa =

        let permutationsCount = 10
        let sorterCount = SorterCount.fromInt 10
        let switchUsesListLength = 5

        let twoCycleList = List.init permutationsCount 
                                     (fun _ -> SorterParts.makeRandomTwoCycle)

        let listOfSwitchUses = 
            List.init switchUsesListLength (fun _ -> 
                        SwitchUses.create SorterParts.switchCount 
                                          SorterParts.switchUseArray)
                      |> Result.sequence 
                      |> Result.ExtractOrThrow
                      
        let sorterList = 
            List.init (SorterCount.value sorterCount)
                      (fun _ -> SorterParts.makeRandomSorter())