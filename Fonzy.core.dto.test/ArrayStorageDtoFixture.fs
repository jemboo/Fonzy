namespace Fonzy.core.dto.test
open Microsoft.VisualStudio.TestTools.UnitTesting
open Newtonsoft.Json

[<TestClass>]
type ArrayStorageDtoFixture () =

    [<TestMethod>]
    member this.SparseIntArrayDto() =
        let testA = [|0;0;1;1;0;1|]
        let expected = testA |> Array.toList
        let sa = testA |> SparseArray.toSparse 0
        let cereal = sa |> SparseIntArrayDto.toJson
        let saB = cereal |> SparseIntArrayDto.fromJson
                         |> Result.ExtractOrThrow

        Assert.AreEqual(sa, saB)


    [<TestMethod>]
    member this.SparseIntArraySetDto() =
        let defaultA = 0;
        let aLen = ArrayLength.fromInt 6

        let testA1 = [|0;0;1;1;0;1|]
        let testA2 = [|1;0;1;0;0;2|]
        let testA3 = [|0;0;1;1;2;1|]
        let testAA = [| testA1; testA2; testA3 |]
        let ssas = testAA |> SparseArraySet.fromStandardArraySet defaultA aLen
        let cereal = ssas |> SparseIntArraySetDto.toJson
        let ssasBk = cereal |> SparseIntArraySetDto.fromJson
                            |> Result.ExtractOrThrow
        Assert.AreEqual(ssas, ssasBk)



    [<TestMethod>]
    member this.DiffIntArrayDto() =
        let baseA = [|1;0;1;0;0;1|]
        let testA1 = [|0;0;1;1;0;1|]
        let testA2 = [|2;2;1;1;0;2|]
        let da1 = testA1 |> DiffArray.toDiff baseA
        let da2 = testA2 |> DiffArray.toDiff baseA

        let cereal1 = da1 |> DiffIntArrayDto.toJson
        let daB1 = cereal1 |> DiffIntArrayDto.fromJson
                           |> Result.ExtractOrThrow

        let cereal2 = da2 |> DiffIntArrayDto.toJson
        let daB2 = cereal2 |> DiffIntArrayDto.fromJson
                           |> Result.ExtractOrThrow

        Assert.AreEqual(da1, daB1)
        Assert.AreEqual(da2, daB2)


    [<TestMethod>]
    member this.DiffIntArraySetDto() =
        let baseA =  [|1;0;1;0;0;1|]
        let aLen = ArrayLength.fromInt 6

        let testA1 = [|0;0;1;1;0;1|]
        let testA2 = [|1;0;1;0;0;2|]
        let testA3 = [|0;0;1;1;2;1|]
        let testAA = [| testA1; testA2; testA3 |]
        let dfas = testAA |> DiffArraySet.fromStandardArraySet baseA
        let cereal = dfas |> DiffIntArraySetDto.toJson
        let dfasBk = cereal |> DiffIntArraySetDto.fromJson
                            |> Result.ExtractOrThrow
        Assert.AreEqual(dfas, dfasBk)

