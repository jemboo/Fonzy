namespace Fonzy.core.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open SortingEval

[<TestClass>]
type ShcFixture () =

    let degree = Degree.fromInt 16
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

    let swPfx = [| |] |> Switch.fromIntArrayAsPerm
                      |> Seq.toArray
    let pfxCt = (SwitchCount.fromInt (swPfx.Length))
    let mutRate = (MutationRate.fromFloat 0.02)
    let mutSpec = sorterMutSpec.Constant 
                            (sorterMutType.ByStage (pfxCt, mutRate))

    let mutSpec2 = sorterMutSpec.Constant 
                            (sorterMutType.ByStage (pfxCt, mutRate))


    let sstAllIntSets = sortableSetType.AllForDegree 
                            (sortableSetRep.Bp64 degree)
    let stageWspec = sorterStageWeightSpec.Constant (StageWeight.fromFloat 1.0)
    let evlSpec = sorterEvalSpec.PerfBin
    let annSpec = annealerSpec.Exp ((Temp.fromFloat 0.00005), 20000.0)
    let updtSpec = shcSaveDetails.BetterThanLast
    let termSpec = shcTermSpec.FixedLength (StepNumber.fromInt 50)
    let srtrShcSpec =
        {
           sorterShcSpec.rngGen = rnG; 
           sorter = srter;
           mutatorSpec = mutSpec;
           srtblSetType = sstAllIntSets;
           sorterStageWeightSpec = stageWspec;
           evalSpec = evlSpec;
           annealerSpec = annSpec;
           loggerSpec = updtSpec;
           termSpec = termSpec;
        }
    let srtrShcSpec2 =
        {
           sorterShcSpec.rngGen = rnG; 
           sorter = srter;
           mutatorSpec = mutSpec;
           srtblSetType = sstAllIntSets;
           sorterStageWeightSpec = stageWspec;
           evalSpec = evlSpec;
           annealerSpec = annSpec;
           loggerSpec = updtSpec;
           termSpec = termSpec;
        }

    [<TestMethod>]
    member this.SHC_id() =
        let id1 = srtrShcSpec |> SorterShcSpec.makeId
        let id2 = srtrShcSpec2 |> SorterShcSpec.makeId
        Assert.AreEqual(id1, id2)


    [<TestMethod>]
    member this.cereal_mutSpec() =
        let id1 = mutSpec |> ByteUtils.base64FromObj
        let msBack = id1 |> ByteUtils.base64ToObj
        Assert.AreEqual(mutSpec, msBack)