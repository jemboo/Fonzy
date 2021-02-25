namespace Fonzy.core.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type CausesFixture () =

    [<TestMethod>]
    member this.CauseFromCauseSpecIntArrayRandGen () =
        let cause = Causes.fromCauseSpec TestData.Causes.causeSpecRandGenIntArray
                        |> Result.ExtractOrThrow
        let env = Enviro.Empty
        let newEnv = cause.op env |> Result.ExtractOrThrow
        let generated = newEnv |> Enviro.toMap 
                               |> ResultMap.read TestData.Causes.genArrayName 
                               |> Result.ExtractOrThrow
        let intDist = generated |> IntDistDto.fromJson |> Result.ExtractOrThrow
        Assert.AreEqual(intDist.vals.Length, TestData.Causes.arrayCount)


    [<TestMethod>]
    member this.CauseFromCauseSpecInt2dArrayRandGen () =
        let cause = Causes.fromCauseSpec TestData.Causes.csLl2dGen 
                                |> Result.ExtractOrThrow
        let env = Enviro.Empty
        let newEnv = cause.op env |> Result.ExtractOrThrow
        let generated = newEnv |> Enviro.toMap 
                               |> ResultMap.read TestData.Causes.genArrayName 
                               |> Result.ExtractOrThrow
        let intDist = generated |> Int2dDistDto.fromJson |> Result.ExtractOrThrow
        Assert.AreEqual(intDist.vals.Length, TestData.Causes.arrayCount)