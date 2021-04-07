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