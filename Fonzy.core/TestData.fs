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
        let goodRefSorter = RefSorter.goodRefSorterForDegree degree
                            |> Result.ExtractOrThrow

        let permSwitchDensity = 0.5
        let sorterLength = degree |> SorterLength.toMediocreRandomPerfLength 
                                                    SwitchOrStage.Stage 
        let sorterCount = SorterCount.fromInt 50
        let makeRandomSorter() = 
                Sorter.createRandom degree sorterLength SwitchFrequency.max iRando

        let makeRandomTwoCycle = 
                TwoCyclePerm.makeRandomTwoCycle 
                                degree iRando permSwitchDensity

        let randomSortableIntArray = 
            SortableIntArray.createRandom degree iRando

        let switchUseArray = Array.init (SwitchCount.value switchCount) 
                                        (fun _ -> iRando.NextPositiveInt)

        let switchList = Switch.switchMap 
                                |> Seq.take (SwitchCount.value switchCount)
                                |> Seq.toList

        let mediocreRandomSorters = List.init (SorterCount.value sorterCount)
                                      (fun _ -> makeRandomSorter())

        let maxConjugatePairs = 100
        let altEvenSorters = List.init maxConjugatePairs (fun stageCt -> 
                    Sorter.makeAltEvenOdd degree (StageCount.fromInt (stageCt + 1)) )
                             |> Result.sequence
                             |> Result.ExtractOrThrow


        let sM () = 
            RefSorter.createRefSorter RefSorter.Green16m

        //let sorterEndM = RefSorter.createRefSorter RefSorter.End16m
        //                 |> Result.ExtractOrThrow
        let sorterGreenM = sM ()
                           |> Result.ExtractOrThrow


    module SortableSet =
        let sortableSetId = SortableSetId.fromGuid (Guid.NewGuid())
        let ssAllIntBits = SortableSetExplicit.allIntBits degree sortableSetId
        let sortableSet = SortableSet.Explicit ssAllIntBits

    module SorterSet = 
        let mediocreSorterSetId = SorterSetId.fromGuid (Guid.NewGuid())
        let altEvenSorterSetId = SorterSetId.fromGuid (Guid.NewGuid())
        let mediocreSorterSet = 
                    SorterSet.fromSorters 
                            mediocreSorterSetId 
                            degree 
                            SorterParts.mediocreRandomSorters

        let altEvenSorterSet = 
                    SorterSet.fromSorters 
                            altEvenSorterSetId
                            degree 
                            SorterParts.altEvenSorters



    module SorterActionRecords =
        let rolloutOfAllBinary = SortableSetRollout.allBinary degree
                                     |> Result.ExtractOrThrow
        let rolloutOfAllSortedBinary = 
                SortableSetRollout.fromSortableIntArrays 
                           degree 
                           (SortableIntArray.allSorted_0_1 degree)
                 |> Result.ExtractOrThrow



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