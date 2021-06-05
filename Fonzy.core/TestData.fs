namespace global
open System

module TestData = 
    let seed = 12374
    let iRando = Rando.fromRngGen (RngGen.createNet seed)
    let degree = Degree.fromInt 8

    module ComboStructures =
        let permutation = Permutation.rotate degree 1


    module SorterParts =
        let switchCount = SwitchCount.fromInt 10

        let deg4Pfx2Sorter = RefSorter.createRefSorter RefSorter.Degree4Prefix2
                             |> Result.ExtractOrThrow

        let goodRefSorter = RefSorter.goodRefSorterForDegree degree
                            |> Result.ExtractOrThrow

        let permSwitchDensity = 0.5
        let sorterLength = degree |> SwitchOrStageCount.toMediocreRandomPerfLength 
                                                    SwitchOrStage.Stage 
        let sorterCount = SorterCount.fromInt 50
        let sorterGen = SorterGen.RandSwitches 
                                    ((SwitchCount.degreeTo999SwitchCount degree),
                                    degree)

        let makeRandomSorter() = 
                SorterGen.createRandom sorterGen iRando

        let makeRandomTwoCycle = 
                TwoCyclePerm.makeRandomTwoCycle 
                                degree iRando permSwitchDensity

        let randomIntBits = 
            IntBits.createRandom degree iRando

        let randomBitsP64 = 
            BitsP64.createRandoms degree iRando 1
                    |> Seq.head

        let switchUseArray = Array.init (SwitchCount.value switchCount) 
                                        (fun _ -> iRando.NextPositiveInt)

        let switchList = Switch.switchMap 
                                |> Seq.take (SwitchCount.value switchCount)
                                |> Seq.toList

        let mediocreRandomSorters = List.init (SorterCount.value sorterCount)
                                      (fun _ -> makeRandomSorter())

        let maxConjugatePairs = 100
        let altEvenSorters = List.init maxConjugatePairs (fun stageCt -> 
                    SorterGen.makeAltEvenOdd degree (StageCount.fromInt (stageCt + 1)) )
                             |> Result.sequence
                             |> Result.ExtractOrThrow


        let sM () = 
            RefSorter.createRefSorter RefSorter.Green16m

        //let sorterEndM = RefSorter.createRefSorter RefSorter.End16m
        //                 |> Result.ExtractOrThrow
        let sorterGreenM = sM ()
                           |> Result.ExtractOrThrow


    module SortableSet =
        let ssBinary = SortableSetBinary.allIntBits degree
        let sortableSet =  ssBinary |> SortableSet.Binary
                                    |> SortableSetSpec.Explicit

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

        let intSetsRolloutAllBinary = IntSetsRollout.allBinary degree
                                      |> Result.ExtractOrThrow

        let bP64SetsRolloutAllBinary = BP64SetsRollout.allBinary degree
                                       |> Result.ExtractOrThrow



        let rolloutOfAllSortedBinary = 
                let ia = IntBits.sorted_0_1_Sequences degree
                            |> Seq.map(fun ia -> {IntBits.values = ia.values })
                IntSetsRollout.fromIntBits 
                               degree
                               ia
                |> Result.ExtractOrThrow

    open SortingEval
    module SortingEvalT = 
        let SorterCoverages() =
            let sorterId1 = SorterId.fromGuid (Guid.NewGuid())
            let sorterId2 = SorterId.fromGuid (Guid.NewGuid())
            let sorterId3 = SorterId.fromGuid (Guid.NewGuid())
            let sorterId4 = SorterId.fromGuid (Guid.NewGuid())
            let sorterId5 = SorterId.fromGuid (Guid.NewGuid())

            let sortableSetId1 = SortableSetId.fromGuid (Guid.NewGuid())
            let switchCount1 = SwitchCount.fromInt 1
            let switchCount2 = SwitchCount.fromInt 2
            let stageCount1 = StageCount.fromInt 1
            let stageCount2 = StageCount.fromInt 2

            let sorterPerf1 = 
                { 
                    sorterPerf.usedSwitchCount = switchCount1;
                    usedStageCount = stageCount1;
                    successful = Some false
                }

            let sorterPerf2 = 
                { 
                    sorterPerf.usedSwitchCount = switchCount1;
                    usedStageCount = stageCount1;
                    successful = Some true
                }

            let sorterPerf3 = 
                    { 
                        sorterPerf.usedSwitchCount = switchCount2;
                        usedStageCount = stageCount2;
                        successful = Some true
                    }

            let sorterPerf4 = 
                { 
                    sorterPerf.usedSwitchCount = switchCount2;
                    usedStageCount = stageCount2;
                    successful = None
                }

            let sorterCoverage1 =
                { 
                    sorterCoverage.sorterId = sorterId1;
                    sortableSetId = sortableSetId1;
                    sorterPerf = sorterPerf1
                }

            let sorterCoverage2 =
                { 
                    sorterCoverage.sorterId = sorterId2;
                    sortableSetId = sortableSetId1;
                    sorterPerf = sorterPerf2
                }

            let sorterCoverage3 =
                { 
                    SortingEval.sorterCoverage.sorterId = sorterId3;
                    SortingEval.sorterCoverage.sortableSetId = sortableSetId1;
                    SortingEval.sorterCoverage.sorterPerf = sorterPerf3
                }

            let sorterCoverage4 =
                { 
                    SortingEval.sorterCoverage.sorterId = sorterId4;
                    SortingEval.sorterCoverage.sortableSetId = sortableSetId1;
                    SortingEval.sorterCoverage.sorterPerf = sorterPerf4
                }

            let sorterCoverage5 =
                { 
                    SortingEval.sorterCoverage.sorterId = sorterId5;
                    SortingEval.sorterCoverage.sortableSetId = sortableSetId1;
                    SortingEval.sorterCoverage.sorterPerf = sorterPerf4
                }


            [sorterCoverage1; sorterCoverage2; sorterCoverage3; sorterCoverage4; sorterCoverage5]




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