namespace Fonzy.world.dto.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type CauseSpecsDtoFixture () =

    [<TestMethod>]
    member this.CauseSpecDto () =
        let genArrayName = "genA"
        let arrayCount = 103
        let randy = RngGen.createLcg (22 |> RandomSeed.fromInt)
        let intDistType = intDistType.Uniform (UniformIntegerDistParams.zeroCentered 5)
        let csIntGen = CauseSpecRandGen.intArray intDistType arrayCount randy genArrayName

        let dto = CauseSpecDto.toDto csIntGen
        let csBack = CauseSpecDto.fromDto dto |> Result.ExtractOrThrow
        Assert.AreEqual(csIntGen, csBack);
