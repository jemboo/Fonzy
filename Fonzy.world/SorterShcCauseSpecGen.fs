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


    let makeMutSpec (mutRate:MutationRate)
                    (steps: StepNumber)
                    (degree:Degree)
                    (dispSorter:sorter)
                    (dispRngGen:RngGen) = 

        let startingTemp = Temp.fromFloat 0.00005
        let stageW = StageWeight.fromFloat 1.0

        let srtbleSetType = sortableSetType.AllForDegree 
                                (sortableSetRep.Integer degree)

        let dispSc = SwitchCount.fromInt (srtbleSetType |> SortableSetType.getPrefix).Length  
        let mutSpec = (dispSc, mutRate) |> sorterMutType.ByStageRfl
                        |> sorterMutSpec.Constant


        let swS = shcStageWeightSpec.Constant stageW
        let evl = sorterEvalSpec.PerfBin
        let ann = annealerSpec.Constant startingTemp
        let updt = shcSaveDetails.Never
        let term = shcTermSpec.FixedLength steps
        {
            sorterShcSpec.rngGen = dispRngGen;
            sorterShcSpec.sorter = dispSorter;
            sorterShcSpec.mutatorSpec = mutSpec;
            sorterShcSpec.srtblSetType = srtbleSetType;
            sorterShcSpec.shcStageWeightSpec = swS;
            evalSpec = evl;
            annealerSpec = ann;
            updaterSpec = updt;
            termSpec = term;
        }



    let makeRunBatchSeq (outputDir:FileDir) 
                        (seed:RandomSeed) =


        let degree = Degree.fromInt 14
        let shcCt = ShcCount.fromInt 50
        let sorterCt = SorterCount.fromInt 50
        let steps = StepNumber.fromInt 5000
        let seedF = (777) |> RandomSeed.fromInt
        let wPfx = []
        let rngF = RngGen.createLcg seedF
        let rng = RngGen.createLcg seed
        let sRndGen = sorterRndGen.RandSymmetric
                                    (wPfx,
                                     (StageCount.degreeTo999StageCount degree),
                                     degree)
        
        let dispSorter = SorterRndGen.createRandom 
                                sRndGen 
                                (rng |> Rando.fromRngGen)


        let ssGen = sorterSetGen.Rnd (sRndGen, rngF, sorterCt)
        let sssrgT = sssrgType.Sorters ssGen


        let sorterShcSpecRndGens = 
            seq {

                    //{ sorterShcSpecRndGen.baseSpec = 
                    //        makeMutSpec
                    //             (MutationRate.fromFloat 0.16)
                    //             steps
                    //             degree dispSorter rng; 
                    //  sorterShcSpecRndGen.sssrgType = sssrgT;
                    //  sorterShcSpecRndGen.count = shcCt;
                    //  sorterShcSpecRndGen.rndGen = rng } 

                    //{ sorterShcSpecRndGen.baseSpec = 
                    //        makeMutSpec
                    //             (MutationRate.fromFloat 0.018)
                    //             steps
                    //             degree dispSorter rng; 
                    //  sorterShcSpecRndGen.sssrgType = sssrgT;
                    //  sorterShcSpecRndGen.count = shcCt;
                    //  sorterShcSpecRndGen.rndGen = rng } 
                  
                    //{ sorterShcSpecRndGen.baseSpec = 
                    //        makeMutSpec
                    //             (MutationRate.fromFloat 0.020)
                    //             steps
                    //             degree dispSorter rng; 
                    //  sorterShcSpecRndGen.sssrgType = sssrgT;
                    //  sorterShcSpecRndGen.count = shcCt;
                    //  sorterShcSpecRndGen.rndGen = rng } 


                    //{ sorterShcSpecRndGen.baseSpec = 
                    //        makeMutSpec
                    //             (MutationRate.fromFloat 0.14)
                    //             steps
                    //             degree dispSorter rng; 
                    //  sorterShcSpecRndGen.sssrgType = sssrgT;
                    //  sorterShcSpecRndGen.count = shcCt;
                    //  sorterShcSpecRndGen.rndGen = rng } 


                    { sorterShcSpecRndGen.baseSpec = 
                            makeMutSpec
                                 (MutationRate.fromFloat 0.18)
                                 steps
                                 degree dispSorter rng; 
                      sorterShcSpecRndGen.sssrgType = sssrgT;
                      sorterShcSpecRndGen.count = shcCt;
                      sorterShcSpecRndGen.rndGen = rng } 


                    { sorterShcSpecRndGen.baseSpec = 
                            makeMutSpec
                                (MutationRate.fromFloat 0.22)
                                steps
                                degree dispSorter rng; 
                      sorterShcSpecRndGen.sssrgType = sssrgT;
                      sorterShcSpecRndGen.count = shcCt;
                      sorterShcSpecRndGen.rndGen = rng } 



                    { sorterShcSpecRndGen.baseSpec = 
                            makeMutSpec
                                (MutationRate.fromFloat 0.26)
                                steps
                                degree dispSorter rng; 
                      sorterShcSpecRndGen.sssrgType = sssrgT;
                      sorterShcSpecRndGen.count = shcCt;
                      sorterShcSpecRndGen.rndGen = rng } 



                    { sorterShcSpecRndGen.baseSpec = 
                               makeMutSpec
                                   (MutationRate.fromFloat 0.3)
                                   steps
                                   degree dispSorter rng; 
                      sorterShcSpecRndGen.sssrgType = sssrgT;
                      sorterShcSpecRndGen.count = shcCt;
                      sorterShcSpecRndGen.rndGen = rng } 

                }

        Console.WriteLine(sprintf "seed: %d" (RandomSeed.value seed))            
        sorterShcSpecRndGens
        |> Seq.mapi(fun dex sg -> 
                            makeTriple 
                               dex outputDir (UseParallel.create false) sg )