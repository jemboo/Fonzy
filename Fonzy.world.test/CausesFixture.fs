namespace Fonzy.world.test
open Microsoft.VisualStudio.TestTools.UnitTesting
open System;


type qq = {a:(obj->unit)->int->int}


[<TestClass>]
type CausesFixture () =

    [<TestMethod>]
    member this.CauseFromCauseSpecIntArrayRandGen () =
        let monitor = fun _ -> (fun _ -> ()) |> Ok
        let cause = Causes.fromCauseSpec TestData.CauseSpec.IntDist.rndUniform
                        |> Result.ExtractOrThrow
        let env = enviro.Empty
        let newEnv = cause.op monitor env |> Result.ExtractOrThrow
        let causedProd =  
            Enviro.getDto<intDistDto> 
                                newEnv 
                                TestData.CauseSpec.IntDist.arrayName
            |> Result.ExtractOrThrow

        let intDist = causedProd |> IntDistDto.fromDto |> Result.ExtractOrThrow
        Assert.AreEqual(intDist.vals.Length, TestData.CauseSpec.IntDist.arrayCount)


    [<TestMethod>]
    member this.CauseFromCauseSpecInt2dArrayRandGen () =
        let monitor = fun _ -> (fun _ -> ()) |> Ok
        let cause = Causes.fromCauseSpec TestData.CauseSpec.IntDist.rnd2dUniform 
                                |> Result.ExtractOrThrow
        let env = enviro.Empty
        let newEnv = cause.op monitor env |> Result.ExtractOrThrow
        let causedProd =  
            Enviro.getDto<int2dDistDto> 
                                newEnv 
                                TestData.CauseSpec.IntDist.arrayName2d
            |> Result.ExtractOrThrow
        let intDist = causedProd |> Int2dDistDto.fromDto |> Result.ExtractOrThrow
        Assert.AreEqual(intDist.vals.Length, TestData.CauseSpec.IntDist.arrayCount)


    [<TestMethod>]
    member this.CauseFromRndGenSorterSet() =
        let monitor = fun _ -> (fun _ -> ()) |> Ok
        let cause = TestData.CauseSpec.SorterSet.rand1  
                    |> Causes.fromCauseSpec
                    |> Result.ExtractOrThrow
        let env = enviro.Empty
        let newEnv = cause.op monitor env |> Result.ExtractOrThrow
        let causedProd =  
            Enviro.getDto<sorterSetDto> 
                                newEnv 
                                TestData.CauseSpec.SorterSet.rndSorterSetName
            |> Result.ExtractOrThrow
        let rndSorterSet = causedProd |> SorterSetDto.fromDto |> Result.ExtractOrThrow
        Assert.AreEqual(rndSorterSet.sorterCount, TestData.CauseSpec.SorterSet.sorterCount)


    [<TestMethod>]
    member this.CauseEvalToSorterPerfBins() =
        let monitor = fun _ -> (fun _ -> ()) |> Ok
        let envO = enviro.Empty
        let causeGen = TestData.CauseSpec.SorterSet.rand1 
                                |> Causes.fromCauseSpec
                                |> Result.ExtractOrThrow
        let envGen = causeGen.op monitor envO |> Result.ExtractOrThrow

        let causeEval = TestData.CauseSpec.SorterSet.evalToSorterPerfBins 
                            |> Causes.fromCauseSpec
                            |> Result.ExtractOrThrow

        let envEvalRes = causeEval.op monitor envGen |> Result.ExtractOrThrow

        let sorterEvalResults =  
             Enviro.getDto<sorterPerfBinDto[]> 
                                envEvalRes
                                TestData.CauseSpec.SorterSet.sorterEvalResultsName
            |> Result.ExtractOrThrow
        Assert.IsTrue(sorterEvalResults.Length > 0)


    [<TestMethod>]
    member this.CauseRndGenToPerfBins() =
        let monitor = fun _ -> (fun _ -> ()) |> Ok
        let causeEvalR = Causes.fromCauseSpec
                                TestData.CauseSpec.SorterSet.rndGenToSorterPerfBins 
                                
        let causeEval = causeEvalR |> Result.ExtractOrThrow
        let resWrldR = World.createFromParent
                                  monitor
                                  World.empty
                                  causeEval

        let resWrld =  resWrldR |> Result.ExtractOrThrow

        let envEvalRes = resWrld.enviro

        let sorterEvalResultsR =  
             Enviro.getDto<sorterPerfBinDto[]> 
                                envEvalRes
                                TestData.CauseSpec.SorterSet.sorterEvalResultsName
        let sorterEvalResults = sorterEvalResultsR |> Result.ExtractOrThrow
        let sbins = sorterEvalResults |> Array.sortByDescending(fun ia -> ia.[2])
        Assert.IsTrue(true)

       
    [<TestMethod>]
    member this.tm() =
        let monitor = fun (x:obj) ->
                        Console.WriteLine(sprintf "%A" x)

        let qqf = fun (mf:obj->unit) (dex:int) -> 
                    mf (dex:> obj)
                    dex * 2

        let qqv = {qq.a = qqf}
        let res = qqv.a monitor 55
        Assert.IsTrue(true)
