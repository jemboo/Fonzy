namespace Fonzy.core.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type ArrayStorageFixture () =

    [<TestMethod>]
    member this.toAndFromSparse() =
      let testA = [|0;0;1;1;0;1|]
      let expected = testA |> Array.toList
      let sa = testA |> SparseArray.toSparse 0
      let aBack = sa |> SparseArray.fromSparse
      let actual = aBack |> Array.toList
      Assert.AreEqual (expected, actual)


    [<TestMethod>]
    member this.toAndFromDiff() =
      let baseA = [|1;0;1;0;0;1|]
      let testA = [|0;0;1;1;0;1|]
      let expected = testA |> Array.toList
      let da = DiffArray.toDiff baseA testA
      let aBack = DiffArray.fromDiff baseA da
      let actual = aBack |> Array.toList
      Assert.AreEqual (expected, actual)



    [<TestMethod>]
     member this.DiffArraySet_fromAndToStandardArraySet() =
        let baseA =  [|1;0;1;0;0;1|]

        let testA1 = [|0;0;1;1;0;1|]
        let testA2 = [|1;0;1;0;0;2|]
        let testA3 = [|0;0;1;1;2;1|]
        let testAA = [| testA1; testA2; testA3 |]
        let eA = testAA |> Array.map(Array.toList) |> Array.toList

        let das = testAA |> DiffArraySet.fromStandardArraySet baseA
        let aABack = das |> DiffArraySet.toStandardArraySet
        let actual = aABack |> Array.map(Array.toList) |> Array.toList
        Assert.AreEqual (eA, actual)



    [<TestMethod>]
     member this.SparseArraySet_fromAndToStandardArraySet() =
        let defaultA = 0;
        let aLen = ArrayLength.fromInt 6

        let testA1 = [|0;0;1;1;0;1|]
        let testA2 = [|1;0;1;0;0;2|]
        let testA3 = [|0;0;1;1;2;1|]
        let testAA = [| testA1; testA2; testA3 |]
        let eA = testAA |> Array.map(Array.toList) |> Array.toList

        let das = testAA |> SparseArraySet.fromStandardArraySet defaultA aLen
        let aABack = das |> SparseArraySet.toStandardArraySet
        let actual = aABack |> Array.map(Array.toList) |> Array.toList
        Assert.AreEqual (eA, actual)
