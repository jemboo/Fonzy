namespace Fonzy.world.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open SortingEval

[<TestClass>]
type ShcFixture () =

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

        let shc = SHC.fromSorterShcSpec TestData.SrtrShcSpec.sscSpec
                  |> Result.ExtractOrThrow

        let shcN = SHC.run shc |> Result.ExtractOrThrow

        shcN.archive |> List.iter(fun a -> 
                    Console.WriteLine(sprintf "%d\t%s\t%f" 
                                (StepNumber.value a.step) 
                                (a.perf |> SorterPerf.report) 
                                (Energy.value a.energy)))

        Assert.IsTrue(true)



    [<TestMethod>]
    member this.SHC_makeBatch() =
        let baseSpec = TestData.SrtrShcSpec.sscSpec
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
                     |> SorterSHCset.makeSorterShcSet

        let batchRes = sHCset.runBatch (UseParallel.create true) shcSet
        let shcRes = SorterSHCset.getResults batchRes
        let txtOut = shcRes |> SorterShcResultsDto.toDto
        Assert.IsTrue(true)