namespace global
open System

module SorterPerfBinGen =
    let rndSortersBaseId = Guid.Parse "00000000-0000-0000-0000-000000000002"

    let makeCauseSpec
                       sorterGen 
                       sorterCount 
                       rndGen 
                       switchUsePlan 
                       sortableSet 
                       useParallel 
                       resultsName =

        CauseSpecSorters.genToSorterPerfBins 
                ("sorterGen", sorterGen)
                ("sorterCount", sorterCount)
                ("rndGen", rndGen)
                ("switchUsePlan", switchUsePlan)
                ("sortableSet", sortableSet)
                ("useParallel", (UseParallel.value useParallel))
                ("resultsName", resultsName)

    
    let sorterCountForDegree (degree:Degree) = 
        if (Degree.value degree) = 16 then
            (SorterCount.fromInt 20000)
        elif (Degree.value degree) = 18 then
            (SorterCount.fromInt 20000)
        elif (Degree.value degree) = 20 then
            (SorterCount.fromInt 10000)
        elif (Degree.value degree) = 22 then
            (SorterCount.fromInt 2000)
        else (SorterCount.fromInt 200)


    let sorterCountForDegreeTest (degree:Degree) = 
        (SorterCount.fromInt 20)

    let degreesToTest = 
         [ 8 .. 2 .. 24 ] |> List.map (Degree.fromInt)

    let buddyStageWindows (degree:Degree) =
        let awys =
            match (Degree.value degree) with
            | 8 ->  [ 1; 2; ]
            | 10 ->  [ 1; 2; 3;]
            | 12 ->  [ 1; 2; 4;]
            | 14 ->  [ 1; 2; 4;]
            | 16 ->  [ 1; 3; 5; ]
            | 18 ->  [1 .. 2 .. 7]
            | 20 ->  [1 .. 2 .. 9]
            | 22 ->  [1 .. 2 .. 11]
            | _ ->   [1 .. 2 .. 13]
        awys |> List.map(StageCount.fromInt)

    let tup999Switches (degree:Degree) =
        ((degree |> SwitchCount.degreeTo999SwitchCount), degree)


    let tup999Stages (degree:Degree) =
        ((degree |> StageCount.degreeTo999StageCount), degree)

    let makeBuddyArgs = 
        let wNd = degreesToTest |> List.map(fun d -> (buddyStageWindows d)
                                                        |> List.map(fun tc -> (tc, d)))
                                |> List.concat
        wNd |> List.map(fun (wc, d) -> ((d |> StageCount.degreeTo999StageCount), wc, d))


    let makeRandSwitches = 
        degreesToTest |> List.map(fun d -> SorterGen.RandSwitches (d |> tup999Switches))

    let makeRandStages = 
        degreesToTest |> List.map(fun d -> SorterGen.RandStages (d |> tup999Stages))

    let makeRandCoComp = 
        degreesToTest |> List.map(fun d -> SorterGen.RandCoComp (d |> tup999Stages))

    let makeRandSymmetric = 
        degreesToTest |> List.map(fun d -> SorterGen.RandSymmetric (d |> tup999Stages))

    let makeRandBuddies = 
        (makeBuddyArgs) |> List.map(SorterGen.RandBuddies)

    let makeRandSymmetricBuddies = 
        (makeBuddyArgs) |> List.map(SorterGen.RandSymmetricBuddies)
