namespace Fonzy.core.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type CausesFixture () =

    [<TestMethod>]
    member this.TestFootage () =
        let om = [("a", "b"); ("b", "b")] |> Map.ofList
        let goo = Guid.Parse "2f8999c9-9656-4701-bb15-ecdf7558a01a"

        Assert.IsTrue(true);
