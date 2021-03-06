namespace Fonzy.core.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type SorterPartsFixture () =

    [<TestMethod>]
    member this.SorterGuids () =
        let sorterCount = 50
        let sorters = Array.init sorterCount 
                                 (fun _ -> TestData.SorterParts.makeRandomSorter())
        let map = sorters |> Array.map(fun s-> ([s :> obj] |> GuidUtils.guidFromObjList), s)
                          |> Map.ofArray
        Assert.IsTrue(map.Count = sorterCount);



    [<TestMethod>]
    member this.SorterSet () =
        let sorterSet = SorterSet.fromSorters TestData.degree
                                TestData.SorterParts.listOfSorters
        Assert.IsTrue(sorterSet.sorterCount = TestData.SorterParts.sorterCount);