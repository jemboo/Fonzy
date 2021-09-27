namespace Fonzy.world.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open SortingEval

[<TestClass>]
type ShcFixture () =

    let degree = Degree.fromInt 16
    let rnG = RngGen.createLcg (RandomSeed.fromInt 9966)
    let iRando = Rando.fromRngGen rnG
    let sorterGen = sorterRndGen.RandStages
                            ([],
                             (StageCount.degreeTo999StageCount degree),
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
    let termSpec = shcTermSpec.FixedLength (StepNumber.fromInt 10)
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

        let mutable shcN = SHC.update shc
                            |> Result.ExtractOrThrow

        let mutable curStep = 0

        for i = 1 to 10000 do
            shcN <- SHC.update shcN
                        |> Result.ExtractOrThrow

            if (RevNumber.value shcN.current.revision ) <> curStep then
                curStep <- (RevNumber.value shcN.current.revision )
                Console.WriteLine(sprintf "%d\t%s\t%s" 
                                    (StepNumber.value shcN.current.step)
                                    (reptP shcN.current)
                                    (engP shcN.current.energy))

        Assert.IsTrue(true)


