namespace Fonzy.world.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open SortingEval

[<TestClass>]
type StoHillClimbFixture () =


    [<TestMethod>]
    member this.Aab() =
        let lone = [1; 2; 3]
        let lapp = 4 :: lone
        let yabb = lapp |> List.head
        Assert.IsTrue(true)


