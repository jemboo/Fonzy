namespace Fonzy.core.dto.test
open Microsoft.VisualStudio.TestTools.UnitTesting
open Newtonsoft.Json

[<TestClass>]
type CombinatoricsTypesDtoFixture () =

    [<TestMethod>]
    member this.PermutationDto() =
        let randy = Rando.fromRngGen (RngGen.createLcg 4321)
        let degree = Degree.fromInt 8
        let perm = Permutation.createRandom degree randy
        let dto = PermutationDto.toDto perm
        let permBack = PermutationDto.fromDto dto |> Result.ExtractOrThrow
        Assert.AreEqual(perm, permBack)

    [<TestMethod>]
    member this.TwoCyclePermDto() =
        let randy = Rando.fromRngGen (RngGen.createLcg 321)
        let degree = Degree.fromInt 8
        let perm = TwoCyclePerm.rndTwoCycle degree 0.85 randy 
        let dto = TwoCyclePermDto.toDto perm
        let permBack = TwoCyclePermDto.fromDto dto |> Result.ExtractOrThrow
        Assert.AreEqual(perm, permBack)