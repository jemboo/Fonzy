namespace global
open System


module TestData = 
    module SorterGa =
        let seed = 1234
        let iRando = Rando.fromRngGen (RngGen.createLcg seed)
        let degree = Degree.fromInt 8
        let permutationsCount = 10
        let sorterLength = SorterLength.degreeToRecordStageCount degree
        let sorter = Sorter.createRandom degree sorterLength None iRando
        let sorterCount = SorterCount.fromInt 10
        let switchCount = SwitchCount.fromInt 10
        let switchUsesListLength = 5
        let permSwitchDensity = 0.5

        let twoCycleList = List.init permutationsCount (fun _ -> 
            TwoCyclePerm.makeRandomTwoCycle degree iRando permSwitchDensity)

        let switchList = Switch.randomSwitchesOfDegree degree iRando
                                |> Seq.take (SwitchCount.value switchCount)
                                |> Seq.toList

        let switchUseArray = Array.init (SwitchCount.value switchCount) 
                                        (fun _ -> iRando.NextPositiveInt)
        
        let arrayOfSwitchUseArrays = 
            let scA (rando:IRando) = 
                Array.init (SwitchCount.value switchCount) 
                           (fun _ -> rando.NextPositiveInt)
            let switchUses (rando:IRando) = 
                SwitchUses.create switchCount (scA rando)
                                |> Result.ExtractOrThrow
            List.init switchUsesListLength
                            (fun _ -> switchUses iRando)

        let sorterList =
            let _sorterF (rando:IRando) = 
                Sorter.createRandom degree sorterLength None rando
            List.init (SorterCount.value sorterCount)
                                   (fun _ -> _sorterF iRando)