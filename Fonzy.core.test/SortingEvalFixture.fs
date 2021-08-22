namespace Fonzy.core.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type SortingEvalFixture () =

    [<TestMethod>]
    member this.SorterPerfBin_fromSorterCoverage() =
        let yak = TestData.SortingEvalT.SorterCoverages()
        let bins = yak |> SortingEval.SorterPerfBin.fromSorterCoverages
        Assert.AreEqual(bins.Length, 2)


    [<TestMethod>]
    member this.test() =

      Assert.IsTrue(true)