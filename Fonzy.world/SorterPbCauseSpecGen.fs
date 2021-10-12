namespace global
open System
open CauseSpec

module SorterPbCauseSpecGen =

    let makeCauseSpec
                sorterRndGen
                sorterCount
                rndGen
                switchUsePlan
                sortableSetType
                sorterSaving
                useParallel
                resultsName =

        CauseSpecSorters.rndGenToPerfBins
                ("sorterRndGen", sorterRndGen)
                ("sorterCount", sorterCount)
                ("rndGen", rndGen)
                ("switchUsePlan", switchUsePlan)
                ("sortableSetType", sortableSetType)
                ("sorterSaving", sorterSaving)
                ("useParallel", (UseParallel.value useParallel))
                ("resultsName", resultsName)

    let sorterCountForDegree (degree:Degree) = 
        if (Degree.value degree) = 8 then
             (SorterCount.fromInt 40000)
        elif (Degree.value degree) = 10 then
             (SorterCount.fromInt 40000)
        elif (Degree.value degree) = 12 then
             (SorterCount.fromInt 40000)
        elif (Degree.value degree) = 14 then
             (SorterCount.fromInt 20000)
        elif (Degree.value degree) = 16 then
             (SorterCount.fromInt 10000)
        elif (Degree.value degree) = 18 then
             (SorterCount.fromInt 50000)
        elif (Degree.value degree) = 20 then
             (SorterCount.fromInt 40000)
        elif (Degree.value degree) = 22 then
             (SorterCount.fromInt 30000)
        else (SorterCount.fromInt 20000)

    let makeTriple (dex:int) 
                   (sorterRndGen:sorterRndGen)
                   (outputDir:FileDir)
                   (u:UseParallel)
                   (randy:IRando) =
        let degree = sorterRndGen |> SorterRndGen.getDegree
        let pfx = sorterRndGen |> SorterRndGen.getSwitchPrefix
        let ssRep = sortableSetRep.Bp64 degree 
        let srtableStType = sortableSetType.SwitchReduced 
                             ((sortableSetType.AllForDegree ssRep), pfx)
        let sorterCount = degree |> sorterCountForDegree
        let rndGen = Rando.nextRnGen randy

        let ssImplBps = SortableSetMaker.allZeroOnes
                                (sortableSetRep.Binary degree)

        let (sortableSetTrim, switchUses) = 
            ssImplBps |> SortingOps.SortableSet.reduceBySorterRndGen sorterRndGen
            |> Result.ExtractOrThrow

        let totalSwitchCt = sorterRndGen |> SorterRndGen.getSwitchCount
        let switchUsePlan = Sorting.SwitchUsePlan.makeIndexes 
                                    switchUses
                                    totalSwitchCt

        let resultsName = "sorterPerfBins"

        let perf10 = ((StageWeight.fromFloat 1.0), (SorterCount.fromInt 10)) |> sorterSaving.Perf
        let causeSpec = makeCauseSpec
                            sorterRndGen 
                            sorterCount 
                            rndGen 
                            switchUsePlan
                            srtableStType
                            perf10
                            u 
                            resultsName

        let csGu = causeSpec.id |> CauseSpecId.value
        let causeSpecDescr = sprintf "%d: Time: %s SorterGen: %s SorterCount: %d" 
                                dex
                                (System.DateTime.Now.ToLongTimeString())
                                (sorterRndGen |> SorterRndGen.reportString csGu) 
                                (SorterCount.value sorterCount)

        (causeSpecDescr, outputDir, causeSpec)



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



    let makeRandomSwitches (degree:Degree) 
                           (degM:int) = 
        let randy = Rando.LcgFromSeed (RandomSeed.fromNow())
        Switch.rndNonDegenSwitchesOfDegree degree randy
                |> Seq.take ((Degree.value degree) * degM)
                |> Seq.toList


    let makeRandomStageSwitches (degree:Degree) 
                                (stageCount:StageCount) = 
        let randy = Rando.LcgFromSeed (RandomSeed.fromNow())
        Stage.rndSeq degree (SwitchFrequency.max) randy
        |> Seq.take (StageCount.value stageCount)
        |> Seq.map(fun st -> st.switches |> List.toSeq)
        |> Seq.concat
        |> Seq.toList


    let makeRandomStageSymSwitches (degree:Degree) 
                                (stageCount:StageCount) = 
        let randy = Rando.LcgFromSeed (RandomSeed.fromNow())
        Stage.rndSymmetric degree randy
        |> Seq.take (StageCount.value stageCount)
        |> Seq.map(fun st -> st.switches |> List.toSeq)
        |> Seq.concat
        |> Seq.toList


    let tup900Switches (switchList: Switch list) 
                       (degree:Degree) =
        (switchList, (degree |> SwitchCount.degreeTo900SwitchCount), degree)


    let tup900Stages (switchList: Switch list) (degree:Degree) =
        (switchList, (degree |> StageCount.degreeTo900StageCount), degree)


    let makeBuddyArgs900 (prefix: Switch list) degreesToTest = 
        makeBuddyArgs prefix
                      degreesToTest 
                      StageCount.degreeTo900StageCount


    let makeRandSwitches900 degreesToTest
                            (degM:int) = 
        degreesToTest |> Seq.map(fun d -> tup900Switches (makeRandomSwitches d degM) d
                                            |> sorterRndGen.RandSwitches)


    let makeRandStages900 degreesToTest 
                          (stageCount:StageCount) =
        degreesToTest |> Seq.map(
                    fun d -> tup900Stages (makeRandomStageSwitches d stageCount) 
                                          d
                              |>  sorterRndGen.RandStages)


    let makeRandSymmetric900 degreesToTest 
                             (stageCount:StageCount) =
        degreesToTest |> Seq.map(
                    fun d -> tup900Stages (makeRandomStageSymSwitches d stageCount) 
                                          d
                              |>  sorterRndGen.RandSymmetric)


    let makeRandBuddies900 degreesToTest =
        (makeBuddyArgs900 [] degreesToTest) |> List.map(sorterRndGen.RandBuddies)

    let makeRandSymmetricBuddies900 degreesToTest =
        (makeBuddyArgs900 [] degreesToTest) |> List.map(sorterRndGen.RandRflBuddies)


    let makeRunBatchSeq (outputDir:FileDir) 
                        (seed:RandomSeed) = 

        let degreesToTest = 
            [ 8; 10; 12; 14; 16; 18; 20; 22; 24;]
             |> Seq.map (Degree.fromInt)

        let allSorterRndGens = 
                  seq { while true do
                 //          yield! (makeRandStages900 degreesToTest (StageCount.fromInt 4))
                 //          yield! (makeRandSymmetric900 degreesToTest (StageCount.fromInt 2))
                           yield! (makeRandSymmetric900 degreesToTest (StageCount.fromInt 4))
                           //yield! (makeRandStages900 degreesToTest (StageCount.fromInt 2))
                     }

        let randy = RngGen.createLcg seed |> Rando.fromRngGen


        allSorterRndGens 
        |> Seq.mapi(fun dex sg -> 
                            makeTriple 
                               dex sg outputDir (UseParallel.create true) randy)
