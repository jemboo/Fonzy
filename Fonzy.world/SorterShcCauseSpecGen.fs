namespace global
open System

module SorterShcCauseSpecGen =

    let makeCauseSpec 
                    sorterShcSpecRndGen
                    useParallel 
                    resultsName =

        CauseSpecSorterShc.sorterShcSpecRndGen
                ("sorterShcSpecRndGen", sorterShcSpecRndGen)
                ("useParallel", (UseParallel.value useParallel))
                ("resultsName", resultsName)

    
    let makeTriple (dex:int)
                   (outputDir:FileDir)
                   (u:UseParallel)
                   (sssRndGen:sorterShcSpecRndGen) = 
        let causeSpecDescr = sprintf "%d: Time: %s " 
                                 dex
                                 (System.DateTime.Now.ToLongTimeString())

        let resultsName = "sorterShcSet"
        let causeSpec = makeCauseSpec sssRndGen u resultsName
        (causeSpecDescr, outputDir, causeSpec)


    let makeBaseSpecVar
                    (startingTemp:Temp)
                    (mutRate:MutationRate)
                    (totSteps: StepNumber)
                    (degree:Degree)
                    (dispSorter:sorter)
                    (dispRngGen:RngGen) 
                    (swPfx:Switch list) = 

        let stageW = StageWeight.fromFloat 1.0

        let srtbleSetTypeB = sortableSetType.AllForDegree 
                                (sortableSetRep.Bp64 degree)


        let srtbleSetType = sortableSetType.SwitchReduced 
                                (srtbleSetTypeB, swPfx)

        let swPfxCt = SwitchCount.fromInt (srtbleSetType |> SortableSetType.getPrefix).Length  
        let mutSpec = (swPfxCt, mutRate) |> sorterMutType.ByStage
                        |> sorterMutSpec.Constant

        let swS = sorterStageWeightSpec.Constant stageW
        let evl = sorterEvalSpec.PerfBin
        let ann = annealerSpec.Constant startingTemp
        let updt = shcSaveDetails.ForSteps (StepNumber.logReporting totSteps)
        let term = shcTermSpec.FixedLength totSteps
        {
            sorterShcSpec.rngGen = dispRngGen;
            sorterShcSpec.sorter = dispSorter;
            sorterShcSpec.mutatorSpec = mutSpec;
            sorterShcSpec.srtblSetType = srtbleSetType;
            sorterShcSpec.sorterStageWeightSpec = swS;
            evalSpec = evl;
            annealerSpec = ann;
            updaterSpec = updt;
            termSpec = term;
        }

    let makeRng (randy:IRando) =
        let seedPond = [|123;234;345;456;567;678;789;890;901;112;223;334;445;556;667;778;889;900;111;222;333;444;555;666;777;888;999|]
                       |> Array.map(RandomSeed.fromInt) |> Array.map(RngGen.createLcg)
        seedPond.[randy.NextPositiveInt % seedPond.Length]

    let makeRunBatchSeq (outputDir:FileDir) 
                        (seed:RandomSeed) =

        let degree = Degree.fromInt 16
        let shcCt = ShcCount.fromInt 10
        let sorterCt = SorterCount.fromInt 10
        let steps = StepNumber.fromInt 10000
        //let seedF = (777) |> RandomSeed.fromInt
       // let seedF = (555) |> RandomSeed.fromInt
        let seedF = (8556) |> RandomSeed.fromInt
        let swPfx = Switch.makeAltEvenOdd degree (StageCount.fromInt 1)
                    |> Result.ExtractOrThrow
                    |> Seq.toList
        let rngF = RngGen.createLcg seedF
        let rng = RngGen.createLcg seed
        let randy = rng |> Rando.fromRngGen

        let sRndGen = sorterRndGen.RandSymmetric
                                    (swPfx,
                                     (StageCount.degreeTo999StageCount degree),
                                     degree)
        
        let dispSorter = SorterRndGen.createRandom 
                                sRndGen 
                                (rng |> Rando.fromRngGen)


        let ssGen = sorterSetGen.Rnd (sRndGen, rngF, sorterCt)
        let sssrgT = sssrgType.Sorters ssGen
        //let sRndGen = sorterRndGen.RandStages
        //                            (swPfx,
        //                             (StageCount.degreeTo999StageCount degree),
        //                             degree)
        
        //let dispSorter = SorterRndGen.createRandom 
        //                        sRndGen 
        //                        (rng |> Rando.fromRngGen)


        let ssGen = sorterSetGen.Rnd (sRndGen, rngF, sorterCt)
        let sssrgT = sssrgType.Sorters ssGen

        let sorterShcSpecRndGens = 
            seq {
                    //{ sorterShcSpecRndGen.baseSpec = 
                    //        makeBaseSpecVar
                    //            (Temp.fromFloat 5.0)
                    //            (MutationRate.fromFloat 0.09)
                    //            steps
                    //            degree dispSorter rng swPfx; 
                    //  sorterShcSpecRndGen.sssrgType = sssrgT;
                    //  sorterShcSpecRndGen.count = shcCt;
                    //  sorterShcSpecRndGen.rndGen = makeRng(randy) } 
            
                    //{ sorterShcSpecRndGen.baseSpec = 
                    //        makeBaseSpecVar
                    //            (Temp.fromFloat 2.0)
                    //            (MutationRate.fromFloat 0.09)
                    //            steps
                    //            degree dispSorter rng swPfx; 
                    //  sorterShcSpecRndGen.sssrgType = sssrgT;
                    //  sorterShcSpecRndGen.count = shcCt;
                    //  sorterShcSpecRndGen.rndGen = makeRng(randy) } 

                    { sorterShcSpecRndGen.baseSpec = 
                            makeBaseSpecVar
                                (Temp.fromFloat 1.0)
                                (MutationRate.fromFloat 0.09)
                                steps
                                degree dispSorter rng swPfx; 
                      sorterShcSpecRndGen.sssrgType = sssrgT;
                      sorterShcSpecRndGen.count = shcCt;
                      sorterShcSpecRndGen.rndGen = makeRng(randy) } 


                    { sorterShcSpecRndGen.baseSpec = 
                            makeBaseSpecVar
                                 (Temp.fromFloat 0.5)
                                 (MutationRate.fromFloat 0.09)
                                 steps
                                 degree dispSorter rng swPfx; 
                      sorterShcSpecRndGen.sssrgType = sssrgT;
                      sorterShcSpecRndGen.count = shcCt;
                      sorterShcSpecRndGen.rndGen = makeRng(randy) } 


                    { sorterShcSpecRndGen.baseSpec = 
                            makeBaseSpecVar
                                (Temp.fromFloat 0.2)
                                (MutationRate.fromFloat 0.09)
                                steps
                                degree dispSorter rng swPfx; 
                      sorterShcSpecRndGen.sssrgType = sssrgT;
                      sorterShcSpecRndGen.count = shcCt;
                      sorterShcSpecRndGen.rndGen = makeRng(randy) } 
            
                    { sorterShcSpecRndGen.baseSpec = 
                        makeBaseSpecVar
                             (Temp.fromFloat 0.1)
                             (MutationRate.fromFloat 0.09)
                             steps
                             degree dispSorter rng swPfx; 
                      sorterShcSpecRndGen.sssrgType = sssrgT;
                      sorterShcSpecRndGen.count = shcCt;
                      sorterShcSpecRndGen.rndGen = makeRng(randy) } 


                    { sorterShcSpecRndGen.baseSpec = 
                          makeBaseSpecVar
                               (Temp.fromFloat 0.05)
                               (MutationRate.fromFloat 0.09)
                               steps
                               degree dispSorter rng swPfx; 
                      sorterShcSpecRndGen.sssrgType = sssrgT;
                      sorterShcSpecRndGen.count = shcCt;
                      sorterShcSpecRndGen.rndGen = makeRng(randy) } 



                    { sorterShcSpecRndGen.baseSpec = 
                            makeBaseSpecVar
                                 (Temp.fromFloat 0.04)
                                 (MutationRate.fromFloat 0.09)
                                 steps
                                 degree dispSorter rng swPfx; 
                      sorterShcSpecRndGen.sssrgType = sssrgT;
                      sorterShcSpecRndGen.count = shcCt;
                      sorterShcSpecRndGen.rndGen = makeRng(randy) } 


                    { sorterShcSpecRndGen.baseSpec = 
                            makeBaseSpecVar
                                 (Temp.fromFloat 0.03)
                                 (MutationRate.fromFloat 0.09)
                                 steps
                                 degree dispSorter rng swPfx; 
                      sorterShcSpecRndGen.sssrgType = sssrgT;
                      sorterShcSpecRndGen.count = shcCt;
                      sorterShcSpecRndGen.rndGen = makeRng(randy) } 

              
                    { sorterShcSpecRndGen.baseSpec = 
                          makeBaseSpecVar
                               (Temp.fromFloat 0.02)
                               (MutationRate.fromFloat 0.02)
                               steps
                               degree dispSorter rng swPfx; 
                      sorterShcSpecRndGen.sssrgType = sssrgT;
                      sorterShcSpecRndGen.count = shcCt;
                      sorterShcSpecRndGen.rndGen = makeRng(randy) } 



                    { sorterShcSpecRndGen.baseSpec = 
                            makeBaseSpecVar
                                 (Temp.fromFloat 0.01)
                                 (MutationRate.fromFloat 0.09)
                                 steps
                                 degree dispSorter rng swPfx; 
                      sorterShcSpecRndGen.sssrgType = sssrgT;
                      sorterShcSpecRndGen.count = shcCt;
                      sorterShcSpecRndGen.rndGen = makeRng(randy) } 


                    { sorterShcSpecRndGen.baseSpec = 
                            makeBaseSpecVar
                                 (Temp.fromFloat 0.005)
                                 (MutationRate.fromFloat 0.09)
                                 steps
                                 degree dispSorter rng swPfx; 
                      sorterShcSpecRndGen.sssrgType = sssrgT;
                      sorterShcSpecRndGen.count = shcCt;
                      sorterShcSpecRndGen.rndGen = makeRng(randy) } 


                    { sorterShcSpecRndGen.baseSpec = 
                            makeBaseSpecVar
                                (Temp.fromFloat 0.0000005)
                                (MutationRate.fromFloat 0.09)
                                steps
                                degree dispSorter rng swPfx; 
                      sorterShcSpecRndGen.sssrgType = sssrgT;
                      sorterShcSpecRndGen.count = shcCt;
                      sorterShcSpecRndGen.rndGen = makeRng(randy) } 




                    //{ sorterShcSpecRndGen.baseSpec = 
                    //        makeBaseSpecVar
                    //            (Temp.fromFloat 0.002)
                    //            (MutationRate.fromFloat 0.15)
                    //            steps
                    //            degree dispSorter rng swPfx; 
                    //  sorterShcSpecRndGen.sssrgType = sssrgT;
                    //  sorterShcSpecRndGen.count = shcCt;
                    //  sorterShcSpecRndGen.rndGen = makeRng(randy) } 

                }

        Console.WriteLine(sprintf "seed: %d" (RandomSeed.value seed))            
        sorterShcSpecRndGens |> Seq.toList |> CollectionUtils.listLoop
        |> Seq.mapi(fun dex sg -> 
                            let sng = {sg with  sorterShcSpecRndGen.rndGen = makeRng(randy)}
                            makeTriple 
                               dex outputDir (UseParallel.create false) sng )