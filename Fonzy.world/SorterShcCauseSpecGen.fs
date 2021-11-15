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
        let mutSpec = (swPfxCt, mutRate) |> sorterMutType.ByStageRfl
                        |> sorterMutSpec.Constant

        let swS = sorterStageWeightSpec.Constant stageW
        let evl = sorterEvalSpec.PerfBin
        let ann = annealerSpec.Constant startingTemp
        let ticsPerLog = 20.0
        let updt = shcSaveDetails.ForSteps (StepNumber.logReporting totSteps ticsPerLog)
        let term = shcTermSpec.FixedLength totSteps
        {
            sorterShcSpec.rngGen = dispRngGen;
            sorterShcSpec.sorter = dispSorter;
            sorterShcSpec.mutatorSpec = mutSpec;
            sorterShcSpec.srtblSetType = srtbleSetType;
            sorterShcSpec.sorterStageWeightSpec = swS;
            evalSpec = evl;
            annealerSpec = ann;
            loggerSpec = updt;
            termSpec = term;
        }

    let makeRng (randy:IRando) =
        let seedPond = [|123;234;345;456;567;678;789;890;901;112;223;334;445;556;667;778;889;900;111;222;333;444;555;666;777;888;999|]
                       |> Array.map(RandomSeed.fromInt) |> Array.map(RngGen.createLcg)
        seedPond.[randy.NextPositiveInt % seedPond.Length]

    let makeRunBatchSeq (outputDir:FileDir) 
                        (seed:RandomSeed) =

        let degree = Degree.fromInt 12
        let shcCt = ShcCount.fromInt 100
        let sorterCt = SorterCount.fromInt 100
        let steps = StepNumber.fromInt 5000
        //let seedF = (777) |> RandomSeed.fromInt
       // let seedF = (555) |> RandomSeed.fromInt
        let seedF = (9556) |> RandomSeed.fromInt
        let refSorter = RefSorter.goodRefSorterForDegree degree |> Result.ExtractOrThrow
        let swPfx2 = refSorter |> Sorter.getSwitchPrefix (StageCount.fromInt 2)
                               |> Seq.toList
        //let swPfx2 = Switch.makeAltEvenOdd degree (StageCount.fromInt 1)
        //            |> Result.ExtractOrThrow
        //            |> Seq.toList
        //let swPfx2 = []
        let rngF = RngGen.createLcg seedF
        let rng = RngGen.createLcg seed
        let randy = rng |> Rando.fromRngGen

        let sRndGen = sorterRndGen.RandSymmetric
                                    (swPfx2,
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


        //let ssGen = sorterSetGen.Rnd (sRndGen, rngF, sorterCt)
        //let sssrgT = sssrgType.Sorters ssGen




        let sorterShcSpecRndGens = 
            seq {

                    { sorterShcSpecRndGen.baseSpec = 
                            makeBaseSpecVar
                                (Temp.fromFloat 0.00)
                                (MutationRate.fromFloat 0.12)
                                steps
                                degree dispSorter rng swPfx2; 
                      sorterShcSpecRndGen.sssrgType = sssrgT;
                      sorterShcSpecRndGen.count = shcCt;
                      sorterShcSpecRndGen.rndGen = makeRng(randy) } 


                    { sorterShcSpecRndGen.baseSpec = 
                            makeBaseSpecVar
                                (Temp.fromFloat 0.00)
                                (MutationRate.fromFloat 0.15)
                                steps
                                degree dispSorter rng swPfx2; 
                      sorterShcSpecRndGen.sssrgType = sssrgT;
                      sorterShcSpecRndGen.count = shcCt;
                      sorterShcSpecRndGen.rndGen = makeRng(randy) } 


                    { sorterShcSpecRndGen.baseSpec = 
                            makeBaseSpecVar
                                (Temp.fromFloat 0.00)
                                (MutationRate.fromFloat 0.18)
                                steps
                                degree dispSorter rng swPfx2; 
                      sorterShcSpecRndGen.sssrgType = sssrgT;
                      sorterShcSpecRndGen.count = shcCt;
                      sorterShcSpecRndGen.rndGen = makeRng(randy) } 

                    { sorterShcSpecRndGen.baseSpec = 
                            makeBaseSpecVar
                                (Temp.fromFloat 0.0005)
                                (MutationRate.fromFloat 0.12)
                                steps
                                degree dispSorter rng swPfx2; 
                      sorterShcSpecRndGen.sssrgType = sssrgT;
                      sorterShcSpecRndGen.count = shcCt;
                      sorterShcSpecRndGen.rndGen = makeRng(randy) } 


                    { sorterShcSpecRndGen.baseSpec = 
                            makeBaseSpecVar
                                (Temp.fromFloat 0.0005)
                                (MutationRate.fromFloat 0.15)
                                steps
                                degree dispSorter rng swPfx2; 
                      sorterShcSpecRndGen.sssrgType = sssrgT;
                      sorterShcSpecRndGen.count = shcCt;
                      sorterShcSpecRndGen.rndGen = makeRng(randy) } 


                    { sorterShcSpecRndGen.baseSpec = 
                            makeBaseSpecVar
                                (Temp.fromFloat 0.0005)
                                (MutationRate.fromFloat 0.18)
                                steps
                                degree dispSorter rng swPfx2; 
                      sorterShcSpecRndGen.sssrgType = sssrgT;
                      sorterShcSpecRndGen.count = shcCt;
                      sorterShcSpecRndGen.rndGen = makeRng(randy) } 

                    { sorterShcSpecRndGen.baseSpec = 
                            makeBaseSpecVar
                                (Temp.fromFloat 0.001)
                                (MutationRate.fromFloat 0.12)
                                steps
                                degree dispSorter rng swPfx2; 
                      sorterShcSpecRndGen.sssrgType = sssrgT;
                      sorterShcSpecRndGen.count = shcCt;
                      sorterShcSpecRndGen.rndGen = makeRng(randy) } 


                    { sorterShcSpecRndGen.baseSpec = 
                            makeBaseSpecVar
                                (Temp.fromFloat 0.001)
                                (MutationRate.fromFloat 0.15)
                                steps
                                degree dispSorter rng swPfx2; 
                      sorterShcSpecRndGen.sssrgType = sssrgT;
                      sorterShcSpecRndGen.count = shcCt;
                      sorterShcSpecRndGen.rndGen = makeRng(randy) } 


                    { sorterShcSpecRndGen.baseSpec = 
                            makeBaseSpecVar
                                (Temp.fromFloat 0.001)
                                (MutationRate.fromFloat 0.18)
                                steps
                                degree dispSorter rng swPfx2; 
                      sorterShcSpecRndGen.sssrgType = sssrgT;
                      sorterShcSpecRndGen.count = shcCt;
                      sorterShcSpecRndGen.rndGen = makeRng(randy) } 


                    { sorterShcSpecRndGen.baseSpec = 
                            makeBaseSpecVar
                                (Temp.fromFloat 0.0015)
                                (MutationRate.fromFloat 0.12)
                                steps
                                degree dispSorter rng swPfx2; 
                      sorterShcSpecRndGen.sssrgType = sssrgT;
                      sorterShcSpecRndGen.count = shcCt;
                      sorterShcSpecRndGen.rndGen = makeRng(randy) } 


                    { sorterShcSpecRndGen.baseSpec = 
                            makeBaseSpecVar
                                (Temp.fromFloat 0.0015)
                                (MutationRate.fromFloat 0.15)
                                steps
                                degree dispSorter rng swPfx2; 
                      sorterShcSpecRndGen.sssrgType = sssrgT;
                      sorterShcSpecRndGen.count = shcCt;
                      sorterShcSpecRndGen.rndGen = makeRng(randy) } 


                    { sorterShcSpecRndGen.baseSpec = 
                            makeBaseSpecVar
                                (Temp.fromFloat 0.0015)
                                (MutationRate.fromFloat 0.18)
                                steps
                                degree dispSorter rng swPfx2; 
                      sorterShcSpecRndGen.sssrgType = sssrgT;
                      sorterShcSpecRndGen.count = shcCt;
                      sorterShcSpecRndGen.rndGen = makeRng(randy) } 


                    { sorterShcSpecRndGen.baseSpec = 
                            makeBaseSpecVar
                                 (Temp.fromFloat 0.0020)
                                 (MutationRate.fromFloat 0.12)
                                 steps
                                 degree dispSorter rng swPfx2; 
                      sorterShcSpecRndGen.sssrgType = sssrgT;
                      sorterShcSpecRndGen.count = shcCt;
                      sorterShcSpecRndGen.rndGen = makeRng(randy) } 


                    { sorterShcSpecRndGen.baseSpec = 
                            makeBaseSpecVar
                                (Temp.fromFloat 0.0020)
                                (MutationRate.fromFloat 0.15)
                                steps
                                degree dispSorter rng swPfx2; 
                      sorterShcSpecRndGen.sssrgType = sssrgT;
                      sorterShcSpecRndGen.count = shcCt;
                      sorterShcSpecRndGen.rndGen = makeRng(randy) } 


                    { sorterShcSpecRndGen.baseSpec = 
                            makeBaseSpecVar
                                (Temp.fromFloat 0.0020)
                                (MutationRate.fromFloat 0.18)
                                steps
                                degree dispSorter rng swPfx2; 
                      sorterShcSpecRndGen.sssrgType = sssrgT;
                      sorterShcSpecRndGen.count = shcCt;
                      sorterShcSpecRndGen.rndGen = makeRng(randy) } 


                    { sorterShcSpecRndGen.baseSpec = 
                            makeBaseSpecVar
                                (Temp.fromFloat 0.0025)
                                (MutationRate.fromFloat 0.12)
                                steps
                                degree dispSorter rng swPfx2; 
                      sorterShcSpecRndGen.sssrgType = sssrgT;
                      sorterShcSpecRndGen.count = shcCt;
                      sorterShcSpecRndGen.rndGen = makeRng(randy) } 
            
                    { sorterShcSpecRndGen.baseSpec = 
                        makeBaseSpecVar
                             (Temp.fromFloat 0.0025)
                             (MutationRate.fromFloat 0.15)
                             steps
                             degree dispSorter rng swPfx2; 
                      sorterShcSpecRndGen.sssrgType = sssrgT;
                      sorterShcSpecRndGen.count = shcCt;
                      sorterShcSpecRndGen.rndGen = makeRng(randy) } 


                    { sorterShcSpecRndGen.baseSpec = 
                          makeBaseSpecVar
                               (Temp.fromFloat 0.025)
                               (MutationRate.fromFloat 0.18)
                               steps
                               degree dispSorter rng swPfx2; 
                      sorterShcSpecRndGen.sssrgType = sssrgT;
                      sorterShcSpecRndGen.count = shcCt;
                      sorterShcSpecRndGen.rndGen = makeRng(randy) } 



                    { sorterShcSpecRndGen.baseSpec = 
                            makeBaseSpecVar
                                 (Temp.fromFloat 0.030)
                                 (MutationRate.fromFloat 0.12)
                                 steps
                                 degree dispSorter rng swPfx2; 
                      sorterShcSpecRndGen.sssrgType = sssrgT;
                      sorterShcSpecRndGen.count = shcCt;
                      sorterShcSpecRndGen.rndGen = makeRng(randy) } 


                    { sorterShcSpecRndGen.baseSpec = 
                            makeBaseSpecVar
                                 (Temp.fromFloat 0.030)
                                 (MutationRate.fromFloat 0.15)
                                 steps
                                 degree dispSorter rng swPfx2; 
                      sorterShcSpecRndGen.sssrgType = sssrgT;
                      sorterShcSpecRndGen.count = shcCt;
                      sorterShcSpecRndGen.rndGen = makeRng(randy) } 

              
                    { sorterShcSpecRndGen.baseSpec = 
                          makeBaseSpecVar
                               (Temp.fromFloat 0.030)
                               (MutationRate.fromFloat 0.18)
                               steps
                               degree dispSorter rng swPfx2; 
                      sorterShcSpecRndGen.sssrgType = sssrgT;
                      sorterShcSpecRndGen.count = shcCt;
                      sorterShcSpecRndGen.rndGen = makeRng(randy) } 



                    //{ sorterShcSpecRndGen.baseSpec = 
                    //        makeBaseSpecVar
                    //             (Temp.fromFloat 0.01)
                    //             (MutationRate.fromFloat 0.2)
                    //             steps
                    //             degree dispSorter rng swPfx; 
                    //  sorterShcSpecRndGen.sssrgType = sssrgT;
                    //  sorterShcSpecRndGen.count = shcCt;
                    //  sorterShcSpecRndGen.rndGen = makeRng(randy) } 


                    //{ sorterShcSpecRndGen.baseSpec = 
                    //        makeBaseSpecVar
                    //             (Temp.fromFloat 0.005)
                    //             (MutationRate.fromFloat 0.2)
                    //             steps
                    //             degree dispSorter rng swPfx; 
                    //  sorterShcSpecRndGen.sssrgType = sssrgT;
                    //  sorterShcSpecRndGen.count = shcCt;
                    //  sorterShcSpecRndGen.rndGen = makeRng(randy) } 


                    //{ sorterShcSpecRndGen.baseSpec = 
                    //        makeBaseSpecVar
                    //            (Temp.fromFloat 0.0000005)
                    //            (MutationRate.fromFloat 0.2)
                    //            steps
                    //            degree dispSorter rng swPfx; 
                    //  sorterShcSpecRndGen.sssrgType = sssrgT;
                    //  sorterShcSpecRndGen.count = shcCt;
                    //  sorterShcSpecRndGen.rndGen = makeRng(randy) } 




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