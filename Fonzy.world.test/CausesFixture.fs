namespace Fonzy.world.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type CausesFixture () =

    [<TestMethod>]
    member this.CauseFromCauseSpecIntArrayRandGen () =
        let cause = Causes.fromCauseSpec TestData.CauseSpec.IntDist.rndUniform
                        |> Result.ExtractOrThrow
        let env = Enviro.Empty
        let newEnv = cause.op env |> Result.ExtractOrThrow
        let causedProd, causedMeta =  
            Enviro.getDtoAndMetaFromEnviro<IntDistDto> 
                                newEnv 
                                TestData.CauseSpec.IntDist.arrayName
            |> Result.ExtractOrThrow

        let intDist = causedProd |> IntDistDto.fromDto |> Result.ExtractOrThrow
        Assert.AreEqual(intDist.vals.Length, TestData.CauseSpec.IntDist.arrayCount)


    [<TestMethod>]
    member this.CauseFromCauseSpecInt2dArrayRandGen () =
        let cause = Causes.fromCauseSpec TestData.CauseSpec.IntDist.rnd2dUniform 
                                |> Result.ExtractOrThrow
        let env = Enviro.Empty
        let newEnv = cause.op env |> Result.ExtractOrThrow
        let causedProd, causedMeta =  
            Enviro.getDtoAndMetaFromEnviro<Int2dDistDto> 
                                newEnv 
                                TestData.CauseSpec.IntDist.arrayName2d
            |> Result.ExtractOrThrow
        let intDist = causedProd |> Int2dDistDto.fromDto |> Result.ExtractOrThrow
        Assert.AreEqual(intDist.vals.Length, TestData.CauseSpec.IntDist.arrayCount)


    [<TestMethod>]
    member this.CauseFromRndGenSorterSet() =
        let cause = Causes.fromCauseSpec TestData.CauseSpec.SorterSet.rand1 
                                |> Result.ExtractOrThrow
        let env = Enviro.Empty
        let newEnv = cause.op env |> Result.ExtractOrThrow
        let causedProd, causedMeta =  
            Enviro.getDtoAndMetaFromEnviro<SorterSetDto> 
                                newEnv 
                                TestData.CauseSpec.SorterSet.rndSorterSetName
            |> Result.ExtractOrThrow
        let rndSorterSet = causedProd |> SorterSetDto.fromDto |> Result.ExtractOrThrow
        Assert.AreEqual(rndSorterSet.sorterCount, TestData.CauseSpec.SorterSet.sorterCount)


    [<TestMethod>]
    member this.CauseEvalToSorterPerfBins() =
        let envO = Enviro.Empty
        let causeGen = Causes.fromCauseSpec TestData.CauseSpec.SorterSet.rand1 
                                |> Result.ExtractOrThrow
        let envGen = causeGen.op envO |> Result.ExtractOrThrow

        let causeEval = Causes.fromCauseSpec 
                                TestData.CauseSpec.SorterSet.evalToSorterPerfBins 
                                |> Result.ExtractOrThrow

        let envEvalRes = causeEval.op envGen |> Result.ExtractOrThrow

        let sorterEvalResults, unusedMeta =  
             Enviro.getDtoAndMetaFromEnviro<SorterPerfBinsDto[]> 
                                envEvalRes
                                TestData.CauseSpec.SorterSet.sorterEvalResultsName
            |> Result.ExtractOrThrow
        Assert.IsTrue(sorterEvalResults.Length > 0)


    [<TestMethod>]
    member this.CauseGenToSorterPerfBins() =
        let envO = Enviro.Empty
        let causeGen = Causes.fromCauseSpec TestData.CauseSpec.SorterSet.rand1 
                                |> Result.ExtractOrThrow
        let envGen = causeGen.op envO |> Result.ExtractOrThrow

        let causeEval = Causes.fromCauseSpec 
                                TestData.CauseSpec.SorterSet.genToSorterPerfBins 
                                |> Result.ExtractOrThrow

        let envEvalRes = causeEval.op envGen |> Result.ExtractOrThrow

        let sorterEvalResults, unusedMeta =  
             Enviro.getDtoAndMetaFromEnviro<SorterPerfBinsDto[]> 
                                envEvalRes
                                TestData.CauseSpec.SorterSet.sorterEvalResultsName
            |> Result.ExtractOrThrow
        Assert.IsTrue(sorterEvalResults.Length > 0)