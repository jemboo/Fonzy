namespace Fonzy.world.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open SortingEval

[<TestClass>]
type StoHillClimbFixture () =

    [<TestMethod>]
    member this.update() =
        let lst = [1;2;3]
        let lst2 = 4::lst
        let h = lst2 |> List.head
        Assert.AreEqual(1, 1)


