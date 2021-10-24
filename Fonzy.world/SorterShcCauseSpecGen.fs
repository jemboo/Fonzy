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
                    (dispPfx:Switch list)
                    (dispSorter:sorter)
                    (dispRngGen:RngGen) = 

        let startingTemp = Temp.fromFloat 0.005
        let stageW = StageWeight.fromFloat 1.0
        let dispSc = SwitchCount.fromInt dispPfx.Length
        let mutSpec = (dispSc, mutRate) |> sorterMutType.ByStageRfl
                        |> sorterMutSpec.Constant
        let srtbleSetType = sortableSetType.AllForDegree 
                                (sortableSetRep.Integer degree)
        let swS = shcStageWeightSpec.Constant stageW
        let evl = sorterEvalSpec.PerfBin
        let ann = annealerSpec.Constant startingTemp
        let updt = shcSaveDetails.Always
        let term = shcTermSpec.FixedLength steps
        {
            sorterShcSpec.rngGen = dispRngGen;
            sorterShcSpec.sorter = dispSorter;
            sorterShcSpec.switchPfx = dispPfx |> List.toArray;
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
        //let degree = Degree.fromInt 16
        //let shcCt = ShcCount.fromInt 100
        //let sorterCt = SorterCount.fromInt 10
        //let steps = StepNumber.fromInt 250
        //let startingTemp = Temp.fromFloat 0.005
        //let stageW = StageWeight.fromFloat 1.0
        //let rng = RngGen.createLcg seed
        //let iRando = rng |> Rando.fromRngGen
        //let sRndGen = sorterRndGen.RandSymmetric
        //                            ([],
        //                             (StageCount.degreeTo999StageCount degree),
        //                             degree)
                                     
        //let wPfx = [||] 
        //let sorter = SorterRndGen.createRandom sRndGen iRando
        //let pfxSc = SwitchCount.fromInt wPfx.Length
        //let mutRate = MutationRate.fromFloat 0.0226
        //let mutSpec = (pfxSc, mutRate) |> sorterMutType.ByStageRfl
        //                |> sorterMutSpec.Constant
        //let srtbleSetType = sortableSetType.AllForDegree 
        //                        (sortableSetRep.Integer degree)
        //let swS = shcStageWeightSpec.Constant stageW
        //let evl = sorterEvalSpec.PerfBin
        //let ann = annealerSpec.Constant startingTemp
        //let updt = shcSaveDetails.Always
        //let term = shcTermSpec.FixedLength steps
        //let baseShcSpec =
        //  {
        //    sorterShcSpec.rngGen = rng;
        //    sorterShcSpec.sorter = sorter;
        //    sorterShcSpec.switchPfx = wPfx;
        //    sorterShcSpec.mutatorSpec = mutSpec;
        //    sorterShcSpec.srtblSetType = srtbleSetType;
        //    sorterShcSpec.shcStageWeightSpec = swS;
        //    evalSpec = evl;
        //    annealerSpec = ann;
        //    updaterSpec = updt;
        //    termSpec = term;
        //  }

        let degree = Degree.fromInt 16
        let shcCt = ShcCount.fromInt 100
        let sorterCt = SorterCount.fromInt 100
        let steps = StepNumber.fromInt 500
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

                    { sorterShcSpecRndGen.baseSpec = 
                            makeMutSpec
                                 (MutationRate.fromFloat 0.024)
                                 steps
                                 degree wPfx dispSorter rng; 
                      sorterShcSpecRndGen.sssrgType = sssrgT;
                      sorterShcSpecRndGen.count = shcCt;
                      sorterShcSpecRndGen.rndGen = rng } 

                    { sorterShcSpecRndGen.baseSpec = 
                            makeMutSpec
                                 (MutationRate.fromFloat 0.026)
                                 steps
                                 degree wPfx dispSorter rng; 
                      sorterShcSpecRndGen.sssrgType = sssrgT;
                      sorterShcSpecRndGen.count = shcCt;
                      sorterShcSpecRndGen.rndGen = rng } 
                  
                    { sorterShcSpecRndGen.baseSpec = 
                            makeMutSpec
                                 (MutationRate.fromFloat 0.028)
                                 steps
                                 degree wPfx dispSorter rng; 
                      sorterShcSpecRndGen.sssrgType = sssrgT;
                      sorterShcSpecRndGen.count = shcCt;
                      sorterShcSpecRndGen.rndGen = rng } 


                    { sorterShcSpecRndGen.baseSpec = 
                            makeMutSpec
                                 (MutationRate.fromFloat 0.030)
                                 steps
                                 degree wPfx dispSorter rng; 
                      sorterShcSpecRndGen.sssrgType = sssrgT;
                      sorterShcSpecRndGen.count = shcCt;
                      sorterShcSpecRndGen.rndGen = rng } 

                    { sorterShcSpecRndGen.baseSpec =
                            makeMutSpec
                                 (MutationRate.fromFloat 0.032)
                                 steps
                                 degree wPfx dispSorter rng; 
                      sorterShcSpecRndGen.sssrgType = sssrgT;
                      sorterShcSpecRndGen.count = shcCt;
                      sorterShcSpecRndGen.rndGen = rng } 

                    { sorterShcSpecRndGen.baseSpec = 
                            makeMutSpec
                                 (MutationRate.fromFloat 0.034)
                                 steps
                                 degree wPfx dispSorter rng; 
                      sorterShcSpecRndGen.sssrgType = sssrgT;
                      sorterShcSpecRndGen.count = shcCt;
                      sorterShcSpecRndGen.rndGen = rng } 


                    { sorterShcSpecRndGen.baseSpec = 
                            makeMutSpec
                                (MutationRate.fromFloat 0.036)
                                steps
                                degree wPfx dispSorter rng; 
                      sorterShcSpecRndGen.sssrgType = sssrgT;
                      sorterShcSpecRndGen.count = shcCt;
                      sorterShcSpecRndGen.rndGen = rng } 



                    { sorterShcSpecRndGen.baseSpec = 
                            makeMutSpec
                                (MutationRate.fromFloat 0.038)
                                steps
                                degree wPfx dispSorter rng; 
                      sorterShcSpecRndGen.sssrgType = sssrgT;
                      sorterShcSpecRndGen.count = shcCt;
                      sorterShcSpecRndGen.rndGen = rng } 


                }

        sorterShcSpecRndGens
        |> Seq.mapi(fun dex sg -> 
                            makeTriple 
                               dex outputDir (UseParallel.create true) sg )