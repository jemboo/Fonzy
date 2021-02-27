namespace Fonzy.core.test

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
        let generated = newEnv |> Enviro.toMap 
                               |> Result.ExtractOrThrow
                               |> ResultMap.read TestData.CauseSpec.IntDist.arrayName 
                               |> Result.ExtractOrThrow
        let intDist = generated |> IntDistDto.fromJson |> Result.ExtractOrThrow
        Assert.AreEqual(intDist.vals.Length, TestData.CauseSpec.IntDist.arrayCount)


    [<TestMethod>]
    member this.CauseFromCauseSpecInt2dArrayRandGen () =
        let cause = Causes.fromCauseSpec TestData.CauseSpec.IntDist.rnd2dUniform 
                                |> Result.ExtractOrThrow
        let env = Enviro.Empty
        let newEnv = cause.op env |> Result.ExtractOrThrow
        let generated = newEnv |> Enviro.toMap
                               |> Result.ExtractOrThrow
                               |> ResultMap.read TestData.CauseSpec.IntDist.arrayName 
                               |> Result.ExtractOrThrow
        let intDist = generated |> Int2dDistDto.fromJson |> Result.ExtractOrThrow
        Assert.AreEqual(intDist.vals.Length, TestData.CauseSpec.IntDist.arrayCount)