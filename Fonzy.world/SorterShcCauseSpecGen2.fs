namespace global
open System

module SorterShcCauseSpecGen2 =

    let makeCauseSpec 
                    sorterShcSpecRndGen
                    useParallel 
                    resultsName =

        CauseSpecSorterShc.sorterShcSpecRndGen
                ("sorterShcSpecRndGen", sorterShcSpecRndGen)
                ("useParallel", (UseParallel.value useParallel))
                ("resultsName", resultsName)

    
    let makeTriple (dex:int)
                   (tup:MutationRate*Temp)
                   (outputDir:FileDir)
                   (u:UseParallel)
                   (sssRndGen:sorterShcSpecRndGen) = 
        let causeSpecDescr = sprintf "%d: Time: %s  Mut: %f Temp: %f" 
                                 dex
                                 (System.DateTime.Now.ToLongTimeString())
                                 (tup|>fst|>MutationRate.value)
                                 (tup|>snd|>Temp.value)
                                 

        let resultsName = "sorterShcSet"
        let causeSpec = makeCauseSpec sssRndGen u resultsName
        (causeSpecDescr, outputDir, causeSpec)


    let makeCauseSpec2 
                    sorterShcSpecRndGen
                    useParallel 
                    resultsName =

        CauseSpecSorterShc.sorterShcSpecRndGen2
                ("sorterShcSpecRndGen", sorterShcSpecRndGen)
                ("useParallel", (UseParallel.value useParallel))
                ("resultsName", resultsName)


    let makeTriple2 (dex:int)
                    (tup:MutationRate*Temp)
                    (outputDir:FileDir)
                    (u:UseParallel)
                    (sssRndGen:sorterShcSpecRndGen) = 
        let causeSpecDescr = sprintf "%d: Time: %s  Mut: %f Temp: %f" 
                                 dex
                                 (System.DateTime.Now.ToLongTimeString())
                                 (tup|>fst|>MutationRate.value)
                                 (tup|>snd|>Temp.value)
                             

        let resultsName = "sorterShcSet"
        let causeSpec = makeCauseSpec2 sssRndGen u resultsName
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
        let mutSpec = (swPfxCt, mutRate) |> sorterMutType.BySwitch
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

    //let makeRng (randy:IRando) =
    //    let seedPond = [|123;234;345;456;567;678;789;890;901;112;223;334;445;556;667;778;889;900;111;222;333;444;555;666;777;888;999|]
    //                   |> Array.map(RandomSeed.fromInt) |> Array.map(RngGen.createLcg)
    //    seedPond.[randy.NextPositiveInt % seedPond.Length]
    let makeRng (randy:IRando) =
        randy.NextPositiveInt |> RandomSeed.fromInt |> RngGen.createLcg

    let seqConfigs () = // [0.09; 0.07; 0.05; 0.04; 0.035; 0.03; 0.0275; 0.025; 0.0225; 0.02; 0.0175; 0.015; 0.013; 0.012; 0.011; 0.01; 0.008; 0.006; 0.004; 0.002; 0.0]
        let temps = [0.01; 0.008;]
                   |> List.map(Temp.fromFloat)
        let muts = [0.0175; 0.02;] |> List.map(MutationRate.fromFloat)
       // let muts = [0.015; 0.0175; 0.02; 0.0225; 0.025; ] |> List.map(MutationRate.fromFloat)

        seq { for t in temps do 
                for m in muts do
                    yield (m, t) }


    let makeRunBatchSeq2 (outputDir:FileDir) 
                         (seed:RandomSeed) =

        let degree = Degree.fromInt 12
        let shcCt = ShcCount.fromInt 20
        let sorterCt = SorterCount.fromInt 2
        let steps = StepNumber.fromInt 1000
        let seedS = (904877) |> RandomSeed.fromInt

        let refSorter = RefSorter.goodRefSorterForDegree degree |> Result.ExtractOrThrow
        let swPfx = refSorter |> Sorter.getSwitchPrefix (StageCount.fromInt 3)
                              |> Seq.toList

        //let swPfx2 = Switch.makeAltEvenOdd degree (StageCount.fromInt 1)
        //            |> Result.ExtractOrThrow
        //            |> Seq.toList


       // let swPfx = []
        let rng = RngGen.createLcg seed
        let rngD = RngGen.createLcg seed
        let rngS = RngGen.createLcg seedS
        let randy = rng |> Rando.fromRngGen

        //let sRndGen = sorterRndGen.RandSymmetric
        //                            (swPfxNone,
        //                            (StageCount.degreeTo999StageCount degree),
        //                             degree)
    
        //let dispSorter = SorterRndGen.createRandom 
        //                        sRndGen 
        //                        (rngD |> Rando.fromRngGen)


        //let ssGen = sorterSetGen.Rnd (sRndGen, rngS, sorterCt)
        //let sssrgT = sssrgType.Sorters ssGen




        //let sRndGen = sorterRndGen.RandStages
        //                            (swPfxNone,
        //                            (StageCount.degreeTo999StageCount degree),
        //                             degree)
    
        //let dispSorter = SorterRndGen.createRandom 
        //                        sRndGen 
        //                        (rng |> Rando.fromRngGen)


        //let ssGen = sorterSetGen.Rnd (sRndGen, rngS, sorterCt)
        //let sssrgT = sssrgType.Sorters ssGen


        let sRndGen = sorterRndGen.RandSwitches
                                    (swPfx,
                                    (SwitchCount.degreeTo999SwitchCount degree),
                                     degree)
    
        let dispSorter = SorterRndGen.createRandom 
                                sRndGen 
                                (rng |> Rando.fromRngGen)


        let ssGen = sorterSetGen.Rnd (sRndGen, rngS, sorterCt)
        let sssrgT = sssrgType.Sorters ssGen




        let sorterShcSpecRndGens = 
            seqConfigs()
                |> Seq.map(
                  fun tup ->
                    (
                        { sorterShcSpecRndGen.baseSpec = 
                            makeBaseSpecVar
                                (snd tup)
                                (fst tup)
                                steps
                                degree dispSorter rng swPfx; 
                          sorterShcSpecRndGen.sssrgType = sssrgT;
                          sorterShcSpecRndGen.count = shcCt;
                          sorterShcSpecRndGen.rndGen = makeRng(randy) 
                          },
                        tup
                    )
                )

        Console.WriteLine(sprintf "seed: %d" (RandomSeed.value seed))            
        sorterShcSpecRndGens |> Seq.toList |> CollectionUtils.listLoop
        |> Seq.mapi(fun dex sg -> 
                            let sng = {(sg |> fst) with  sorterShcSpecRndGen.rndGen = makeRng(randy)}
                            makeTriple2 
                               dex (sg |> snd) outputDir (UseParallel.create false) sng )