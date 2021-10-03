namespace Fonzy.world.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open SortingEval

[<TestClass>]
type ShcFixture () =

    let degree = Degree.fromInt 18
    let rnG = RngGen.createLcg (RandomSeed.fromInt 366)
    let iRando = Rando.fromRngGen rnG
    //let sorterGen = sorterRndGen.RandStages
    //                        ([],
    //                         (StageCount.degreeTo999StageCount degree),
    //                         degree)
    let sorterGen = sorterRndGen.RandSwitches
                            ([],
                             (SwitchCount.degreeTo999SwitchCount degree),
                             degree)
    let srter = SorterRndGen.createRandom sorterGen iRando
    //let srter = RefSorter.goodRefSorterForDegree degree
    //            |> Result.ExtractOrThrow

    let swPfx = [| |] |> Switch.fromIntArray
                      |> Seq.toArray
    let pfxCt = (SwitchCount.fromInt (swPfx.Length))
    let mutRate = (MutationRate.fromFloat 0.02)
    let mutSpec = sorterMutSpec.Constant 
                            (sorterMutationType.ByStage (pfxCt,mutRate))
    let sstAllIntSets = sortableSetType.AllForDegree 
                            (sortableSetRep.Bp64 degree)
    let stageWspec = shcStageWeightSpec.Constant (StageWeight.fromFloat 1.0)
    let evlSpec = sorterEvalSpec.PerfBin
    let annSpec = annealerSpec.Exp ((Temp.fromFloat 0.00005), 20000.0)
    let updtSpec = shcSaveDetails.BetterThanLast
    let termSpec = shcTermSpec.FixedLength (StepNumber.fromInt 250)
    let srtrShcSpec =
        {
           sorterShcSpec.rngGen = rnG; 
           sorter = srter;
           switchPfx = swPfx;
           mutatorSpec = mutSpec;
           srtblSetType = sstAllIntSets;
           shcStageWeightSpec = stageWspec;
           evalSpec = evlSpec;
           annealerSpec = annSpec;
           updaterSpec = updtSpec;
           termSpec = termSpec;
        }

    [<TestMethod>]
    member this.SHC_update() =

        let reptP (sshc:sorterShc) =
            match sshc.perf with
            | Some p -> p |> SorterPerf.report
            | None -> ""

        let engP eo =
            match eo with
            | Some e -> sprintf "%f" (Energy.value e)
            | None -> ""

        let shc = SorterShcSpec.toShc srtrShcSpec
                    |> Result.ExtractOrThrow

        //let mutable shcN = SHC.update shc
        //                    |> Result.ExtractOrThrow

        //let mutable curRev = 0

        //for i = 1 to 5000 do
        //    shcN <- SHC.update shcN
        //                |> Result.ExtractOrThrow

        let shcN = SHC.run shc |> Result.ExtractOrThrow

        shcN.archive |> List.iter(fun a -> 
                    Console.WriteLine(sprintf "%d\t%s\t%f" 
                                (StepNumber.value a.step) 
                                (a.perf |> SorterPerf.report) 
                                (Energy.value a.energy)))

            //if (RevNumber.value shcN.current.revision ) <> curRev then
            //    curRev <- (RevNumber.value shcN.current.revision )
            //    Console.WriteLine(sprintf "%d\t%s\t%s" 
            //                        (StepNumber.value shcN.current.step)
            //                        (reptP shcN.current)
            //                        (engP shcN.current.energy))

        Assert.IsTrue(true)



    [<TestMethod>]
    member this.SHC_makeBatch() =
        let baseSpec = srtrShcSpec
        let rnGn = RngGen.createLcg (RandomSeed.fromInt 376)
        let sssrgT = sssrgType.RndGen
        let count = ShcCount.fromInt 5
        let sssrg = { 
                        sorterShcSpecRndGen.baseSpec = baseSpec;
                        sorterShcSpecRndGen.sssrgType = sssrgT;
                        sorterShcSpecRndGen.rndGen = rnGn;
                        sorterShcSpecRndGen.count = count;
                    }
        let shcSet = SorterShcSpecRndGen.generate None None sssrg
                     |> Result.ExtractOrThrow
                     |> Seq.map(SorterShcSpec.toShc)
                     |> Seq.toList
                     |> Result.sequence
                     |> Result.ExtractOrThrow
                     |> List.toArray

        let batchRes = SHC.runBatch shcSet
        Assert.IsTrue(true)
