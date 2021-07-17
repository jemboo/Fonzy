namespace global
open System

module TestData =
    let seed = 123
    //let degreeW = Degree.fromInt 8
    let rnGen = RngGen.createLcg (RandomSeed.fromInt seed)
    let randy = Rando.fromRngGen rnGen
    let nextRnGen() =
        RngGen.createLcg (RandomSeed.fromInt randy.NextPositiveInt)

    module SortableSet =
        let sorterSetGenId1 = SortableSetId.fromGuid (Guid.Parse "20000000-0000-0000-0000-000000000222")
        let sorterSetGenId2 = SortableSetId.fromGuid (Guid.Parse "22000000-0000-0000-0000-000000000222")
        let sortableCount = SortableCount.fromInt 5
        let sortableCount2 = SortableCount.fromInt 6
        let rndBits = SortableSetGenerated.rndBits sorterSetGenId1 TestData.degree sortableCount rnGen
        let rndBits2 = SortableSetGenerated.rndBits sorterSetGenId2 TestData.degree sortableCount2 rnGen

    module CauseSpec =

        module IntDist =
            let arrayName = "arrayName"
            let arrayName2d = "arrayName2d"
            let arrayCount = 103


            let intDistType = IntDistType.Uniform (UniformIntegerDistParams.zeroCentered 5)
            let rndUniform = CauseSpecRandGen.intArray 
                                                intDistType arrayCount 
                                                (nextRnGen()) arrayName
            
            let int2dDistType = Int2dDistType.Uniform (UniformInt2dDistParams.square 5)
            let rnd2dUniform = CauseSpecRandGen.int2dArray 
                                        int2dDistType arrayCount 
                                        (nextRnGen()) arrayName2d

        module SorterSet =
            let count = 10
            //let rndSortersName = "rndSortersName"
            let testResultsName = "testResultsName"
            let sortableSetName = "sortableSetName"
            let rndSorterSetName = "rndSorterSetName"
            let sorterEvalResultsName = "sorterEvalResults"
            let sorterSetId1 = SorterSetId.fromGuid (Guid.Parse "10000000-0000-0000-0000-000000000222")
            let sorterSetId2 = SorterSetId.fromGuid (Guid.Parse "11000000-0000-0000-0000-000000000222")
            let sorterSetId3 = SorterSetId.fromGuid (Guid.Parse "11100000-0000-0000-0000-000000000222")
            let sorterEvalId = SorterSetId.fromGuid (Guid.Parse "11110000-0000-0000-0000-000000000222")

            let intDistType = IntDistType.Normal (NormalIntegerDistParams.zeroCentered 1.0)
            let stageCount = StageCount.degreeTo999StageCount TestData.degree
            let switchCount = stageCount |>  StageCount.toSwitchCount TestData.degree
            let switchPrefixCount = (Degree.value TestData.degree) / 2 
                                     |> SwitchCount.fromInt
            let switchUsePlanIndexes = Sorting.SwitchUsePlan.makeIndexes 
                                        switchPrefixCount
                                        switchCount

            let sorterGen = SorterGen.RandStages (stageCount, TestData.degree)
            let swPfx = TestData.degree |> TwoCycleGen.evenMode |> Switch.fromTwoCyclePerm |> Seq.toList
            let sorterRndGen = sorterRndGen.RandStages (swPfx, stageCount, TestData.degree)
            let switchFreq = SwitchFrequency.max
            let sorterCount = SorterCount.fromInt 100
            let useParallel = false


            let srgPr ssid sg sc rng ssn =
                CauseSpecSorters.rndGen ("sorterSetId", ssid)
                                        ("sorterGen", sg)
                                        ("sorterCount", sc)
                                        ("rndGen", rng)
                                        (rndSorterSetName, ssn)

            let rand1 = srgPr sorterSetId1 sorterGen 
                             sorterCount (nextRnGen()) rndSorterSetName
            let rand2 = srgPr sorterSetId2 sorterGen 
                             sorterCount (nextRnGen()) rndSorterSetName
            let rand3 = srgPr sorterSetId3 sorterGen
                             sorterCount (nextRnGen()) rndSorterSetName

            let switchUsePlan = Sorting.SwitchUsePlan.All

            let evalMush d ssn sup sbset up resn =
                CauseSpecSorters.evalToSorterPerfBins 
                                        ("degree", d)
                                        ("sorterSetName", ssn)
                                        ("switchUsePlan", sup)
                                        ("sortableSetSpec", sbset)
                                        ("useParallel", up)
                                        ("resultsName", resn)

            let evalToSorterPerfBins = 
                    evalMush
                            TestData.degree 
                            rndSorterSetName 
                            switchUsePlan 
                            TestData.SortableSet.sortableSet
                            useParallel
                            sorterEvalResultsName


            let genMush sg sc rng sup sbset up resn =
                CauseSpecSorters.genToSorterPerfBins 
                                        ("sorterGen", sg)
                                        ("sorterCount", sc)
                                        ("rndGen", rng)
                                        ("switchUsePlan", sup)
                                        ("sortableSetSpec", sbset)
                                        ("useParallel", up)
                                        ("resultsName", resn)

            let rndGenMush rsg sc rng sup sbset up resn =
                CauseSpecSorters.rndGenToPerfBins 
                                        ("rndSorterGen", rsg)
                                        ("sorterCount", sc)
                                        ("rndGen", rng)
                                        ("switchUsePlan", sup)
                                        ("sortableSetSpec", sbset)
                                        ("useParallel", up)
                                        ("resultsName", resn)


            let genToSorterPerfBins = 
                    genMush
                            sorterGen
                            sorterCount
                            (nextRnGen())
                            switchUsePlan 
                            TestData.SortableSet.sortableSet
                            useParallel
                            sorterEvalResultsName


            let rndGenToSorterPerfBins = 
                    rndGenMush
                            sorterRndGen
                            sorterCount
                            (nextRnGen())
                            switchUsePlanIndexes 
                            TestData.SortableSet.sortableSet
                            useParallel
                            sorterEvalResultsName


    module WorldAction =
        module IntDist =
            let causeRndUniform = Causes.fromCauseSpec CauseSpec.IntDist.rndUniform
                                    |> Result.ExtractOrThrow
            let randomUniform = WorldAction.create World.empty causeRndUniform

        module SorterGen = 
            let randCause = Causes.fromCauseSpec CauseSpec.SorterSet.rand1 
                            |> Result.ExtractOrThrow
            let randWorldAction = WorldAction.create World.empty randCause

    module World = 
    
        let world1Id = Guid.Parse "00000000-0000-0000-0000-000000000881"
        let world2Id = Guid.Parse "00000000-0000-0000-0000-000000000882"
        let world1ParentId = WorldId.fromGuid (Guid.Parse "00000000-0000-0000-0000-000000000771")
        let world2ParentId = WorldId.fromGuid (Guid.Parse "00000000-0000-0000-0000-000000000772")

        let map1 = [("key11","val11"); ("key12","val12"); ("key13","val13"); ("key14","val14")]
                   |> Map.ofList

        let map2 = [("key21","val21"); ("key22","val22"); ("key23","val23"); ("key24","val24")]
                   |> Map.ofList

        let enviro1 = Enviro.ObjectMap map1
        let enviro2 = Enviro.ObjectMap map2

        let world1 = World.create world1ParentId Causes.noOp enviro1
        let world2 = World.create world2ParentId Causes.noOp enviro2


    module WorldMerge =
        let world1Name = "world1"
        let world2Name = "world2"
        let mergedWorldId = WorldMergeId.fromGuid (Guid.Parse "00000000-0000-0000-0000-000000000991")

        let sourceWorldsNameMap = [(world1Name, World.world1); 
                                   (world2Name, World.world2);]
                                            |> Map.ofList

        let mm1 = {MergeMapItem.sourceMapName="world1"; 
                   MergeMapItem.sourceMapKey="key11"; 
                   outputKey="key11"}

        let mm2 = {MergeMapItem.sourceMapName="world2"; 
                   MergeMapItem.sourceMapKey="key22"; 
                   outputKey="key22"}

        let mapM = [("keyM1","valM1");] |> Map.ofList
        let enviroM = Enviro.ObjectMap mapM
        let mergeMapItems = [mm1;mm2]
        





    //let getId (job:Job) = 
    //    match job with
    //    | GetWorld w -> w.id
    //    | MakeWorld wa -> wa.childId

    //let getParentId (job:Job) = 
    //    match job with
    //    | GetWorld w -> w.parentId
    //    | MakeWorld wa -> Some wa.parentWorld.id

    //let getCause (job:Job) = 
    //    match job with
    //    | GetWorld w -> w.cause
    //    | MakeWorld wa -> wa.cause
