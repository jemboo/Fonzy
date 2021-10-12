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


    let makeRunBatchSeq (outputDir:FileDir) 
                        (seed:RandomSeed) =
        let degree = Degree.fromInt 16
        let shcCt = ShcCount.fromInt 100
        let steps = StepNumber.fromInt 1000
        let startingTemp = Temp.fromFloat 0.005
        let stageW = StageWeight.fromFloat 2.0
        let rng = RngGen.createLcg seed
        let iRando = rng |> Rando.fromRngGen
        let sRndGen = sorterRndGen.RandSymmetric
                                    ([],
                                     (StageCount.degreeTo999StageCount degree),
                                     degree)
                                     
        let wPfx = [||] 
        let sorter = SorterRndGen.createRandom sRndGen iRando
        let pfxSc = SwitchCount.fromInt wPfx.Length
        let mutRate = MutationRate.fromFloat 0.04
        let mutSpec = (pfxSc, mutRate) |> sorterMutationType.ByStageRfl
                        |> sorterMutSpec.Constant
        let srtbleSetType = sortableSetType.AllForDegree 
                                (sortableSetRep.Integer degree)
        let swS = shcStageWeightSpec.Constant stageW
        let evl = sorterEvalSpec.PerfBin
        let ann = annealerSpec.Constant startingTemp
        let updt = shcSaveDetails.Always
        let term = shcTermSpec.FixedLength steps
        let baseShcSpec =
          {
            sorterShcSpec.rngGen = rng;
            sorterShcSpec.sorter = sorter;
            sorterShcSpec.switchPfx = wPfx;
            sorterShcSpec.mutatorSpec = mutSpec;
            sorterShcSpec.srtblSetType = srtbleSetType;
            sorterShcSpec.shcStageWeightSpec = swS;
            evalSpec = evl;
            annealerSpec = ann;
            updaterSpec = updt;
            termSpec = term;
          }

        let sssrgT = sssrgType.RndGen


        let sorterShcSpecRndGens = 
            seq {
                { sorterShcSpecRndGen.baseSpec = baseShcSpec; 
                  sorterShcSpecRndGen.sssrgType = sssrgT;
                  sorterShcSpecRndGen.count = shcCt;
                  sorterShcSpecRndGen.rndGen = rng } }

        sorterShcSpecRndGens
        |> Seq.mapi(fun dex sg -> 
                            makeTriple 
                               dex outputDir (UseParallel.create true) sg )