namespace Fonzy.core.dto.test
open Microsoft.VisualStudio.TestTools.UnitTesting
open Newtonsoft.Json

[<TestClass>]
type RandoDtoFixture () =

    [<TestMethod>]
    member this.RngGenDto() =
        let rngGen = {RngGen.rngType=RngType.Lcg; seed = RandomSeed.create "" 123|>Result.ExtractOrThrow}
        let dto = RngGenDto.toDto rngGen
        let rngGenBack = RngGenDto.fromDto dto |> Result.ExtractOrThrow
        Assert.AreEqual(rngGen, rngGenBack)