namespace global
open System

module SorterPbCauseSpecGen2 =

    let makeCauseSpec
                sorterRndGen
                sorterCount
                rndGen
                switchUsePlan
                sortableSetSpec
                useParallel
                resultsName =

        CauseSpecSorters.rndGenToPerfBins
                ("sorterRndGen", sorterRndGen)
                ("sorterCount", sorterCount)
                ("rndGen", rndGen)
                ("switchUsePlan", switchUsePlan)
                ("sortableSetSpec", sortableSetSpec)
                ("useParallel", (UseParallel.value useParallel))
                ("resultsName", resultsName)

    let makeRandomSwitches (degree:Degree) = 
        let randy = Rando.LcgFromSeed (RandomSeed.fromNow)
        Switch.rndSwitchesOfDegree degree randy
                |> Seq.take (Degree.value degree)
                |> Seq.toList
    
    let sorterCountForDegree (degree:Degree) = 

        if (Degree.value degree) = 8 then
             (SorterCount.fromInt 2000000)
        elif (Degree.value degree) = 10 then
             (SorterCount.fromInt 2000000)
        elif (Degree.value degree) = 12 then
             (SorterCount.fromInt 2000000)
        elif (Degree.value degree) = 14 then
             (SorterCount.fromInt 1000000)
        elif (Degree.value degree) = 16 then
             (SorterCount.fromInt 500000)
        elif (Degree.value degree) = 18 then
             (SorterCount.fromInt 250000)
        elif (Degree.value degree) = 20 then
             (SorterCount.fromInt 200000)
        elif (Degree.value degree) = 22 then
             (SorterCount.fromInt 150000)
        else (SorterCount.fromInt 100000)


    let sorterCountForDegreeTest (degree:Degree) = 
        (SorterCount.fromInt 2)


    let buddyStageWindows (degree:Degree) =
        let awys =
            match (Degree.value degree) with
            | 8 ->  [ 1; 2; 3; ]
            | 10 ->  [ 1; 2; 3; 4; ]
            | 12 ->  [ 1; 2; 3; 4; 5; ]
            | 14 ->  [ 1; 3; 3; 4; 5; 6; ]
            | 16 ->  [ 1; 2; 3; 4; 5; 6; 7; ]
            | 18 ->  [ 1; 2; 3; 4; 5; 6; 7; 8; ]
            | 20 ->  [ 1; 3; 5; 7; 9; ]
            | 22 ->  [ 1; 3; 5; 7; 9; ]
            | _ ->   [ 1; 3; 5; 7; 9; 11;]
        awys |> List.map(StageWindowSize.fromInt)


    let makeBuddyArgs (prefix: Switch list) 
                      degreesToTest 
                      (stageCtr:Degree->StageCount) = 

        let wNd = degreesToTest 
                    |> List.map(fun d -> 
                        (buddyStageWindows d) |> List.map(fun tc -> (tc, d)))
                    |> List.concat
        wNd |> List.map(fun (wc, d) -> (prefix, (d |> stageCtr), wc, d))


    let tup900Switches (switchList: Switch list) (degree:Degree) =
        (switchList, (degree |> SwitchCount.degreeTo900SwitchCount), degree)

    let tup900Stages (switchList: Switch list) (degree:Degree) =
        (switchList, (degree |> StageCount.degreeTo900StageCount), degree)

    let makeBuddyArgs900 (prefix: Switch list) degreesToTest = 
        makeBuddyArgs prefix
                      degreesToTest 
                      StageCount.degreeTo900StageCount

    let makeRandSwitches900 degreesToTest =
        degreesToTest |> List.map(fun d -> tup900Switches (makeRandomSwitches d) d
                                            |> sorterRndGen.RandSwitches)

    let makeRandStages900 degreesToTest =
        degreesToTest |> List.map((tup900Stages []) >> sorterRndGen.RandStages)

    let makeRandSymmetric900 degreesToTest =
        degreesToTest |> List.map((tup900Stages []) >> sorterRndGen.RandSymmetric)

    let makeRandBuddies900 degreesToTest =
        (makeBuddyArgs900 [] degreesToTest) |> List.map(sorterRndGen.RandBuddies)

    let makeRandSymmetricBuddies900 degreesToTest =
        (makeBuddyArgs900 [] degreesToTest) |> List.map(sorterRndGen.RandSymmetricBuddies)



    let makeRunBatchSeq (seed:RandomSeed) 
                        (outputDir:FilePath)= 

        let degreesToTest = 
            [ 8; 10; 12; 14; 16; 18; 20; 22; 24;] //14; 16; 22; 24;]
            //   [ 14; 16; 18; 24;]
             |> List.map (Degree.fromInt)

        let allSorterRndGens = 
                //(makeRandSwitches degreesToTest) |> List.append
                // (makeRandStages degreesToTest) |> List.append
                // (makeRandCoComp degreesToTest) |> List.append
                // (makeRandSymmetric degreesToTest) |> List.append
                 //(makeRandStages999 degreesToTest) |> List.append
                 // (makeRandStages900 degreesToTest) |> List.append
                  (makeRandSwitches900 degreesToTest)



        let randy = RngGen.createLcg seed |> Rando.fromRngGen
        let nextRnGen(randy:IRando) =
            RngGen.createLcg (RandomSeed.fromInt randy.NextPositiveInt)

        let sorterRndGenToCauseSpec (dex:int) 
                                    (sorterRndGen:sorterRndGen) =

            let degree = sorterRndGen |> SorterRndGen.getDegree
            let sorterCount = degree |> sorterCountForDegree
            let rndGen = (nextRnGen(randy))


            let sortableSetSpec = SortableSetGenerated.allBp64 degree
                                    |> SortableSetSpec.Generated

            //let sortableSetSpec = SortableSetGenerated.allIntBits degree
            //                        |> SortableSetSpec.Generated

            let sortableSetEx = sortableSetSpec 
                                    |> SortableSetSpec.getSortableSetExplicit
                                    |> Result.ExtractOrThrow

            let (sortableSetTrim, switchUses) = 
                        sortableSetEx |> SortingOps.SortableSet.reduce 
                                                        sorterRndGen


            let totalSwitchCt = sorterRndGen |> SorterRndGen.getSwitchCount
            let switchUsePlan = Sorting.SwitchUsePlan.makeIndexes 
                                        switchUses
                                        totalSwitchCt

            let useParallel = UseParallel.create true
            let resultsName = "sorterPerfBins"
            let pfxDescr = "pfxDescr"
            let causeSpecDescr = sprintf "%d: Time: %s SorterGen: %s SorterCount: %d" 
                                    dex
                                    (System.DateTime.Now.ToLongTimeString())
                                    (sorterRndGen |> SorterRndGen.reportString) 
                                    (SorterCount.value sorterCount)

            let causeSpec = makeCauseSpec
                                sorterRndGen 
                                sorterCount 
                                rndGen 
                                switchUsePlan 
                                sortableSetSpec 
                                useParallel 
                                resultsName

            (causeSpecDescr, outputDir, causeSpec)

        allSorterRndGens |> CollectionUtils.listLoop
                         |> Seq.mapi(fun dex sg -> sorterRndGenToCauseSpec dex sg)
