namespace Fonzy.core.dto.test
open Microsoft.VisualStudio.TestTools.UnitTesting
open Newtonsoft.Json

[<TestClass>]
type MathDataDtoFixture () =

    [<TestMethod>]
    member this.IntDistDto() =
        let rngGen = {RngGen.rngType=RngType.Lcg; seed = RandomSeed.create "" 123|>Result.ExtractOrThrow}
        let rndy = Rando.fromRngGen rngGen
        let idt = intDistType.Uniform {uniformIntegerDistParams.min = 0; max= 1}
        let intD = IntDist.makeRandom idt rndy 10
        let intDDto = IntDistDto.toDto intD
        let cereal = Json.serialize intDDto
        let intDtoBack = Json.deserialize<intDistDto> cereal |> Result.ExtractOrThrow
        let intDBack = IntDistDto.fromDto intDtoBack  |> Result.ExtractOrThrow
        Assert.AreEqual(intD, intDBack)


    [<TestMethod>]
    member this.Int2dDistDto() =
        let rngGen = {RngGen.rngType=RngType.Lcg; seed = RandomSeed.create "" 123|>Result.ExtractOrThrow}
        let rndy = Rando.fromRngGen rngGen
        let l2dDist = int2dDistType.Uniform (UniformInt2dDistParams.square 10)
        let l2dD = Int2dDist.makeRandom l2dDist rndy 10
        let l2dDDto = Int2dDistDto.toDto l2dD
        let cereal = Json.serialize l2dDDto
        let l2dDistDtoBack = Json.deserialize<int2dDistDto> cereal |> Result.ExtractOrThrow
        let l2dDDistBack = Int2dDistDto.fromDto l2dDistDtoBack  |> Result.ExtractOrThrow
        Assert.AreEqual(l2dD, l2dDDistBack)