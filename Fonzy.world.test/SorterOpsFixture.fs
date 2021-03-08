namespace Fonzy.world.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type SorterOpsFixture () =

    [<TestMethod>]
    member this.SortAllFull() =
        let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
        let sorter16 = RefSorter.CreateRefSorter RefSorter.Green16 |> Result.ExtractOrThrow
        let sortableSet = SortableSetRollup.allBinary degree |> Result.ExtractOrThrow
       
        let res = SorterEval.fullRollup sorter16 sortableSet
        let w = fst res       

        let resR = SorterEval.fullRollupR sorter16 sortableSet
        let wR = resR    

        Assert.IsTrue(true)

