namespace global
open System

module TestData = 
    let seed = 123 |> RandomSeed.fromInt
    let iRando = Rando.fromRngGen (RngGen.createNet seed)
    let degree = Degree.fromInt 10

    module ComboStructures =
        let permutation = Permutation.rotate degree 1


    module SorterParts =
        let switchCount = SwitchCount.fromInt 10

        let deg4Pfx2Sorter = RefSorter.createRefSorter RefSorter.Degree4Prefix2
                             |> Result.ExtractOrThrow

        let goodRefSorter = RefSorter.goodRefSorterForDegree degree
                            |> Result.ExtractOrThrow

        let twoStageSorter = Sorter.fromSwitches 
                                goodRefSorter.degree
                                (goodRefSorter.switches
                                    |> Seq.take (Degree.value goodRefSorter.degree)
                                    |> Seq.toArray)

        let fourStageSorter = Sorter.fromSwitches 
                                goodRefSorter.degree
                                (goodRefSorter.switches
                                    |> Seq.take ((Degree.value goodRefSorter.degree) * 2)
                                    |> Seq.toArray)

        let sorterSegment = Sorter.fromSwitches 
                                goodRefSorter.degree
                                (goodRefSorter.switches
                                    |> Seq.skip (Degree.value goodRefSorter.degree)
                                    |> Seq.take (Degree.value goodRefSorter.degree)
                                    |> Seq.toArray)


        let permSwitchDensity = 0.5
        let sorterLength = degree |> SwitchOrStageCount.toMediocreRandomPerfLength 
                                                    SwitchOrStage.Stage 
        let sorterCount = SorterCount.fromInt 20
        let sorterRndGen = sorterRndGen.RandSwitches 
                                    ([],
                                     (SwitchCount.degreeTo999SwitchCount degree),
                                     degree)

        let makeRandomSorter() = 
                SorterRndGen.createRandom sorterRndGen iRando

        let makeRandomTwoCycle = 
                TwoCyclePerm.rndTwoCycle 
                                degree 
                                permSwitchDensity
                                iRando 

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

        let sorterGreenM = RefSorter.createRefSorter RefSorter.Green16m
                           |> Result.ExtractOrThrow


    module SortableSet =
        let ssBinary = SortableSetBinary.allIntBits degree
        let sortableSet =  ssBinary |> sortableSetO.Binary
                                    |> sortableSetSpec.Explicit

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

        let intSetsRolloutOfAll = IntSetsRollout.allBinary degree
                                |> Result.ExtractOrThrow

        let bP64SetsRolloutOfAll = BP64SetsRollout.allBinary degree
                                 |> Result.ExtractOrThrow

        let intSetsRolloutOfAllSorted = 
                let ia = IntBits.sorted_0_1_Sequences degree
                            |> Seq.map(fun ia -> {intBits.values = ia.values })
                IntSetsRollout.fromIntBits 
                               degree
                               ia
                |> Result.ExtractOrThrow


        let bp64SetsRolloutOfAllSorted = 
                let ia = IntBits.sorted_0_1_Sequences degree
                            |> Seq.map(fun ia -> {intBits.values = ia.values })
                BP64SetsRollout.fromIntBits
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
                    perf = sorterPerf1;
                    usedSwitches = [||];
                }

            let sorterCoverage2 =
                { 
                    sorterCoverage.sorterId = sorterId2;
                    sortableSetId = sortableSetId1;
                    perf = sorterPerf2;
                    usedSwitches = [||];
                }

            let sorterCoverage3 =
                { 
                    SortingEval.sorterCoverage.sorterId = sorterId3;
                    SortingEval.sorterCoverage.sortableSetId = sortableSetId1;
                    SortingEval.sorterCoverage.perf = sorterPerf3;
                    usedSwitches = [||];
                }

            let sorterCoverage4 =
                { 
                    SortingEval.sorterCoverage.sorterId = sorterId4;
                    SortingEval.sorterCoverage.sortableSetId = sortableSetId1;
                    SortingEval.sorterCoverage.perf = sorterPerf4;
                    usedSwitches = [||];
                }

            let sorterCoverage5 =
                { 
                    SortingEval.sorterCoverage.sorterId = sorterId5;
                    SortingEval.sorterCoverage.sortableSetId = sortableSetId1;
                    SortingEval.sorterCoverage.perf = sorterPerf4;
                    usedSwitches = [||];
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
                        SwitchUses.init SorterParts.switchUseArray)
        
        let sorterList = 
                [ RefSorter.createRefSorter RefSorter.Green16m |> Result.ExtractOrThrow;
                  RefSorter.createRefSorter RefSorter.Degree16 |> Result.ExtractOrThrow;
                  RefSorter.createRefSorter RefSorter.End16 |> Result.ExtractOrThrow; ]


        let sorterSet = 
            SorterSet.fromSorters
                (SorterSetId.fromGuid (Guid.Empty))
                (Degree.fromInt 16)
                sorterList
