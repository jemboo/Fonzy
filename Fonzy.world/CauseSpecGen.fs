namespace global
open System

module SorterPerfBinGen =

    let rndSortersBaseId = Guid.Parse "00000000-0000-0000-0000-000000000002"

    let makeCauseSpec
                sorterGen
                sorterCount
                rndGen
                switchUsePlan
                sortableSetSpec
                useParallel
                resultsName =

        CauseSpecSorters.genToSorterPerfBins 
                ("sorterGen", sorterGen)
                ("sorterCount", sorterCount)
                ("rndGen", rndGen)
                ("switchUsePlan", switchUsePlan)
                ("sortableSetSpec", sortableSetSpec)
                ("useParallel", (UseParallel.value useParallel))
                ("resultsName", resultsName)

    
    let sorterCountForDegree (degree:Degree) = 

        if (Degree.value degree) = 8 then
             (SorterCount.fromInt 200000)
        elif (Degree.value degree) = 10 then
             (SorterCount.fromInt 200000)
        elif (Degree.value degree) = 12 then
             (SorterCount.fromInt 200000)
        elif (Degree.value degree) = 14 then
             (SorterCount.fromInt 200000)
        elif (Degree.value degree) = 16 then
             (SorterCount.fromInt 100000)
        elif (Degree.value degree) = 18 then
             (SorterCount.fromInt 50000)
        elif (Degree.value degree) = 20 then
             (SorterCount.fromInt 20000)
        elif (Degree.value degree) = 22 then
             (SorterCount.fromInt 4000)
        else (SorterCount.fromInt 2000)


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
        awys |> List.map(StageCount.fromInt)


    let makeBuddyArgs degreesToTest (stageCtr:Degree->StageCount) = 
        let wNd = degreesToTest 
                    |> List.map(fun d -> 
                        (buddyStageWindows d) |> List.map(fun tc -> (tc, d)))
                    |> List.concat
        wNd |> List.map(fun (wc, d) -> ((d |> stageCtr), wc, d))


    let tup900Switches (degree:Degree) =
        ((degree |> SwitchCount.degreeTo900SwitchCount), degree)

    let tup900Stages (degree:Degree) =
        ((degree |> StageCount.degreeTo900StageCount), degree)

    let makeBuddyArgs900 degreesToTest = 
        makeBuddyArgs degreesToTest 
                      StageCount.degreeTo900StageCount

    let makeRandSwitches900 degreesToTest =
        degreesToTest |> List.map(tup900Switches >> SorterGen.RandSwitches)

    let makeRandStages900 degreesToTest =
        degreesToTest |> List.map(tup900Stages >> SorterGen.RandStages)

    let makeRandCoComp900 degreesToTest =
        degreesToTest |> List.map(tup900Stages >> SorterGen.RandCoComp)

    let makeRandSymmetric900 degreesToTest =
        degreesToTest |> List.map(tup900Stages >> SorterGen.RandSymmetric)

    let makeRandBuddies900 degreesToTest =
        (makeBuddyArgs900 degreesToTest) |> List.map(SorterGen.RandBuddies)

    let makeRandSymmetricBuddies900 degreesToTest =
        (makeBuddyArgs900 degreesToTest) |> List.map(SorterGen.RandSymmetricBuddies)



    let tup999Switches (degree:Degree) =
        ((degree |> SwitchCount.degreeTo999SwitchCount), degree)

    let tup999Stages (degree:Degree) =
        ((degree |> StageCount.degreeTo999StageCount), degree)

    let makeBuddyArgs999 degreesToTest = 
        makeBuddyArgs degreesToTest 
                      StageCount.degreeTo999StageCount
        
    let makeRandSwitches999 degreesToTest =
        degreesToTest |> List.map(tup999Switches >> SorterGen.RandSwitches)

    let makeRandStages999 degreesToTest =
        degreesToTest |> List.map(tup999Stages >> SorterGen.RandStages)

    let makeRandCoComp999 degreesToTest =
        degreesToTest |> List.map(tup999Stages >> SorterGen.RandCoComp)

    let makeRandSymmetric999 degreesToTest =
        degreesToTest |> List.map(tup999Stages >> SorterGen.RandSymmetric)

    let makeRandBuddies999 degreesToTest =
        (makeBuddyArgs999 degreesToTest) |> List.map(SorterGen.RandBuddies)

    let makeRandSymmetricBuddies999 degreesToTest =
        (makeBuddyArgs999 degreesToTest) |> List.map(SorterGen.RandSymmetricBuddies)


    let makeRunBatchSeq (seed:int) 
                        (outputDir:string)= 
        let randy = RngGen.createLcg seed |> Rando.fromRngGen
        let nextRnGen(randy:IRando) =
            RngGen.createLcg randy.NextPositiveInt

        let degreesToTest = 
            [ 8; 10; 12; 14; 16; 18; 20; 22; 24;] //14; 16; 22; 24;]
            //   [ 14; 16; 18; 24;]
             |> List.map (Degree.fromInt)

        let allSorterGens = 
                //(makeRandSwitches degreesToTest) |> List.append
                // (makeRandStages degreesToTest) |> List.append
                // (makeRandCoComp degreesToTest) |> List.append
                // (makeRandSymmetric degreesToTest) |> List.append
                 //(makeRandStages999 degreesToTest) |> List.append
                 // (makeRandStages900 degreesToTest) |> List.append
                  (makeRandSwitches900 degreesToTest)

        let mcsW (dex:int) (sorterGen:SorterGen) = 
            let degree = sorterGen |> SorterGen.getDegree
            let sorterCount = degree |> sorterCountForDegree
            let rndGen = (nextRnGen(randy))
            let switchUsePlan = Sorting.SwitchUsePlan.All
            let sortableSetSpec = SortableSetGenerated.allBp64 degree
                                    |> SortableSetSpec.Generated
            let useParallel = UseParallel.create true
            let resultsName = "sorterPerfBins"

            let causeSpecDescr = sprintf "%d: Time: %s SorterGen: %s SorterCount: %d" 
                                    dex
                                    (System.DateTime.Now.ToLongTimeString())
                                    (sorterGen |> SorterGen.reportString) 
                                    (SorterCount.value sorterCount)

            let causeSpec = makeCauseSpec
                                sorterGen 
                                sorterCount 
                                rndGen 
                                switchUsePlan 
                                sortableSetSpec 
                                useParallel 
                                resultsName

            (causeSpecDescr, outputDir, causeSpec)

        allSorterGens |> CollectionUtils.listLoop
                      |> Seq.mapi(fun dex sg -> mcsW dex sg)
